{
  description = "An example NixOS configuration";
  inputs = {
    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixos-generators = {
      url = "github:/nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-filter.url = "github:numtide/nix-filter";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = {
    self,
    home,
    nixpkgs,
    nixos-generators,
    nixos-hardware,
    deploy-rs,
    nix-filter,
    agenix,
  }: let
    rev =
      if self ? rev
      then self.rev
      else "dirty";

    fix-nixpkgs-path = import ./modules/fix-nixpkgs-path.nix {inherit nixpkgs;};
    all-overlays =
      [
        nix-filter.overlays.default
        deploy-rs.overlay
      ]
      ++ import ./overlays;
    aarch64-linuxPkgs = import nixpkgs {
      system = "aarch64-linux";
      overlays =
        all-overlays
        ++ [
          (final: prev: {
            makeModulesClosure = x:
              prev.makeModulesClosure (x // {allowMissing = true;});
          })
        ];
    };
    aarch64-darwinPkgs = import nixpkgs {
      system = "aarch64-darwin";
      overlays = all-overlays;
      config.allowUnfree = true;
    };
    x86_64Pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = all-overlays;
      config.allowUnfree = true;
    };
    packages = rec {
      aarch64-linux.raspberryPiInstaller = with ARCTURUS;
        nixos-generators.nixosGenerate {
          inherit modules;
          pkgs = aarch64-linuxPkgs;
          format = "sd-aarch64-installer";
        };
      x86_64-linux.writeRaspberryPiFlash = with x86_64Pkgs;
        writeScriptBin "write-raspberry-pi-flash" ''
          path_to_image=$(cat ${aarch64-linux.raspberryPiInstaller}/nix-support/hydra-build-products | cut -d ' ' -f 3)
          ${zstd}/bin/zstd -d --stdout $path_to_image | ${coreutils}/bin/dd of=$1 bs=4096 conv=fsync status=progress
        '';
      x86_64-linux.toolbox = x86_64Pkgs.toolbox;
      aarch64-darwin.toolbox = aarch64-darwinPkgs.toolbox;
      aarch64-linux.toolbox = aarch64-linuxPkgs.toolbox;
    };

    MALAK2 = (import ./machines/MALAK2) {
      inherit all-overlays home fix-nixpkgs-path rev;
    };
    EZRA = (import ./machines/EZRA) {
      inherit all-overlays fix-nixpkgs-path rev agenix;
    };
    ARCTURUS = (import ./machines/ARCTURUS) {
      inherit rev;
      hardware-module = nixos-hardware.nixosModules.raspberry-pi-4;
    };
  in {
    inherit packages;
    homeConfigurations = {
      d4hines = home.lib.homeManagerConfiguration {
        pkgs = aarch64-darwinPkgs;
        modules = import ./machines/DARESH {
          inherit rev;
          pkgs = aarch64-darwinPkgs;
        };
      };
      malak = home.lib.homeManagerConfiguration {
        pkgs = x86_64Pkgs;
        modules = import ./machines/MALAK {
          inherit rev;
          pkgs = x86_64Pkgs;
        };
      };
    };

    nixosConfigurations = {
      # My desktop
      MALAK2 = nixpkgs.lib.nixosSystem MALAK2;
      # My raspberry pi
      ARCTURUS = nixpkgs.lib.nixosSystem ARCTURUS;
      # Server
      EZRA = nixpkgs.lib.nixosSystem EZRA;
    };
    apps.x86_64-linux.writeRaspberryPiFlash = {
      type = "app";
      program = "${self.packages.x86_64-linux.writeRaspberryPiFlash}/bin/write-raspberry-pi-flash";
    };
    # deploy.nodes.ARCTURUS = {
    #   hostname = "192.168.0.103";
    #   profiles.system = {
    #     user = "root";
    #     path = deploy-rs.lib.aarch64-linux.activate.nixos self.nixosConfigurations.ARCTURUS;
    #   };
    # };
    deploy.nodes.EZRA = {
      hostname = "ezra.hines.house";
      profiles.system = {
        user = "root";
        path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.EZRA;
        remoteBuild = false;
      };
    };
    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
    formatter.aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.alejandra;
  };
}

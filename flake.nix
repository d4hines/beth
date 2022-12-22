{
  description = "An example NixOS configuration";
  inputs = {
    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixos-22.05-aarch64";
    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-vscode-server.url = "github:MatthewCash/nixos-vscode-server";
    nixos-vscode-server.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators = {
      url = "github:/nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    deploy-rs.url = "github:serokell/deploy-rs";
    nix-filter.url = "github:numtide/nix-filter";
    vimconfig.url = "path:./vimconfig";
  };
  outputs = {
    self,
    home,
    nixpkgs,
    nixpkgs-darwin,
    dream2nix,
    nixos-vscode-server,
    nixos-generators,
    nixos-hardware,
    deploy-rs,
    nix-filter,
    vimconfig,
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
        vimconfig.overlays.default
      ]
      ++ (import ./overlays {inherit dream2nix;});
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
    aarch64-darwinPkgs = import nixpkgs-darwin {
      system = "aarch64-darwin";
      overlays = all-overlays;
    };
    x86_64Pkgs = import nixpkgs {system = "x86_64-linux";};
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
    };

    RADAH = (import ./machines/RADAH) {
      inherit nixos-vscode-server all-overlays home fix-nixpkgs-path rev;
    };
    ARCTURUS = (import ./machines/ARCTURUS) {
      inherit rev;
      hardware-module = nixos-hardware.nixosModules.raspberry-pi-4;
    };
  in {
    inherit packages;
    homeConfigurations.d4hines = home.lib.homeManagerConfiguration {
      pkgs = aarch64-darwinPkgs;
      modules = import ./machines/DARESH;
    };
    nixosConfigurations = {
      # My desktop
      RADAH = nixpkgs.lib.nixosSystem RADAH;
      # My raspberry pi
      ARCTURUS = nixpkgs.lib.nixosSystem ARCTURUS;
    };
    apps.x86_64-linux.writeRaspberryPiFlash = {
      type = "app";
      program = "${self.packages.x86_64-linux.writeRaspberryPiFlash}/bin/write-raspberry-pi-flash";
    };
    deploy.nodes.ARCTURUS = {
      hostname = "arcturus.local";
      profiles.system = {
        user = "root";
        path = deploy-rs.lib.aarch64-linux.activate.nixos self.nixosConfigurations.ARCTURUS;
      };
    };
    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
    formatter.aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.alejandra;
  };
}

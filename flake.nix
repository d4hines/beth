{
  description = "An example NixOS configuration";
  inputs = {
    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib = {
      url = "github:xmonad/xmonad-contrib";
      inputs.xmonad.follows = "xmonad";
    };
    nixos-vscode-server.url = "github:MatthewCash/nixos-vscode-server";
    nixos-vscode-server.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators = {
      url = "github:/nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    gh-stack = {
      url = "github:d4hines/gh-stack";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs.url = "github:serokell/deploy-rs";
    complice-xmobar = {
      url = "path:./machines/ARCTURUS/complice-xmobar";
      inputs.dream2nix.follows = "dream2nix";
    };
    scripts = {
      url = "path:./machines/RADAH/scripts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.dream2nix.follows = "dream2nix";
    };
    neovitality = {
      url = "path:./neovitality";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { self
    , home
    , nixpkgs
    , dream2nix
    , xmonad
    , xmonad-contrib
    , nixos-vscode-server
    , nixos-generators
    , nixos-hardware
    , gh-stack
    , deploy-rs
    , complice-xmobar
    , scripts
    , neovitality
    }:
    let
      fix-nixpkgs-path = import ./modules/fix-nixpkgs-path.nix { inherit nixpkgs; };
      external-overlays = [ xmonad.overlay xmonad-contrib.overlay deploy-rs.overlay ];
      RADAH = (import ./machines/RADAH) {
        inherit nixos-vscode-server external-overlays home gh-stack scripts fix-nixpkgs-path;
      };
      ARCTURUS = (import ./machines/ARCTURUS) {
        inherit complice-xmobar;
        hardware-module = nixos-hardware.nixosModules.raspberry-pi-4;
      };
    in
    {
      homeConfigurations.d4hines = home.lib.homeManagerConfiguration (import ./machines/DARESH { neovim = neovitality.defaultPackage.aarch64-darwin; });
      nixosConfigurations = {
        # My desktop
        RADAH = nixpkgs.lib.nixosSystem RADAH;
        # My raspberry pi
        ARCTURUS = nixpkgs.lib.nixosSystem ARCTURUS;
      };
      packages =
        let
          aarch64Pkgs = import nixpkgs {
            system = "aarch64-linux";
            overlays = [
              (final: prev: {
                makeModulesClosure = x:
                  prev.makeModulesClosure (x // { allowMissing = true; });
              })
            ];
          };
          x86_64Pkgs = import nixpkgs { system = "x86_64-linux"; };
        in
        rec {
          aarch64-linux.raspberryPiInstaller = with ARCTURUS; nixos-generators.nixosGenerate {
            inherit modules;
            pkgs = aarch64Pkgs;
            format = "sd-aarch64-installer";
          };
          x86_64-linux.writeRaspberryPiFlash = with x86_64Pkgs; writeScriptBin "write-raspberry-pi-flash" ''
            path_to_image=$(cat ${aarch64-linux.raspberryPiInstaller}/nix-support/hydra-build-products | cut -d ' ' -f 3)
            ${zstd}/bin/zstd -d --stdout $path_to_image | ${coreutils}/bin/dd of=$1 bs=4096 conv=fsync status=progress
          '';
        };
      apps.x86_64-linux.writeRaspberryPiFlash = { type = "app"; program = "${self.packages.x86_64-linux.writeRaspberryPiFlash}/bin/write-raspberry-pi-flash"; };
      deploy.nodes.ARCTURUS = {
        hostname = "arcturus.local";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.aarch64-linux.activate.nixos self.nixosConfigurations.ARCTURUS;
        };
      };
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}

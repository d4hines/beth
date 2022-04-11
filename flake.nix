{
  description = "An example NixOS configuration";
  inputs = {
    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    npm-build-package.url = "github:serokell/nix-npm-buildpackage";
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
  };
  outputs =
    { self
    , home
    , nixpkgs
    , npm-build-package
    , xmonad
    , xmonad-contrib
    , nixos-vscode-server
    , nixos-generators
    , nixos-hardware
    }:
    let overlay-module = ({ pkgs, ... }: {
      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays =
        [ npm-build-package.overlay xmonad.overlay xmonad-contrib.overlay ]
        ++ (import ./overlays)
      ;
    });
    in
    {
      nixosConfigurations = {
        RADAH = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            overlay-module

            ./modules/configuration.nix
            ./modules/sound.nix
            ./modules/cron.nix
            ./modules/containers.nix
            home.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.d4hines = { ... }: {
                imports = [
                  ./modules/home
                  nixos-vscode-server.nixosModules.home-manager.nixos-vscode-server
                  ./modules/home/nixos-only.nix
                ];
              };
            }
          ];
        };
      };
      #  nix run github:nix-community/home-manager --no-write-lock-file -- switch --flake .#d4hines"
      homeConfigurations.d4hines = home.lib.homeManagerConfiguration {
        homeDirectory = "/home/d4hines";
        username = "d4hines";
        system = "x86_64-linux";
        configuration = import ./modules/home;
        extraModules = [ overlay-module ./modules/home/arch-only.nix ];
      };
      packages.aarch64-linux = {
        # Usage:
        # nix build .#packages.aarch64-linux.raspberryPiInstaller
        # zstd -d --stdout ./result/sd-image/<image file> | dd of=<sd card> bs=4096 conv=fsync status=progress
        #
        # TODO: write writeScriptBin package that automates this.
        # I can just use `cat result/nix-support/hydra-build-products | cut -d ' ' -f 3` to get the file name
        raspberryPiInstaller = nixos-generators.nixosGenerate {
          pkgs = (import nixpkgs {
            system = "aarch64-linux";
            overlays = [
              (final: prev: {
                makeModulesClosure = x:
                  prev.makeModulesClosure (x // { allowMissing = true; });
              })
            ];
          });
          format = "sd-aarch64-installer";
          modules = [
            nixos-hardware.nixosModules.raspberry-pi-4
            ({ pkgs, ... }:
              {
                fileSystems = {
                  "/" = {
                    device = "/dev/disk/by-label/NIXOS_SD";
                    fsType = "ext4";
                    options = [ "noatime" ];
                  };
                };
                networking = {
                  hostName = "ARCTURUS";
                  # TODO: something about this isn't working, so I just plugged into ethernet
                  wireless =
                    let
                      ssid = builtins.readFile ./secrets/ssid;
                      psk = builtins.readFile ./secrets/wifi_psk;
                    in
                    {
                      enable = true;
                      networks."${ssid}".psk = psk;
                      interfaces = [ "wlan0" ];
                    };
                };
                environment.systemPackages = with pkgs; [ vim ];
                services.openssh = {
                  enable = true;
                  passwordAuthentication = false;
                };
                networking.firewall.allowedTCPPorts = [ 7000 ];
                users = {
                  mutableUsers = false;
                  users."d4hines" = {
                    isNormalUser = true;
                    hashedPassword = builtins.readFile ./secrets/password;
                    extraGroups = [ "wheel" ];
                    openssh.authorizedKeys.keyFiles = [ ./keys/authorized_keys ];
                  };
                };
              })
          ];
        };
      };
    };
}

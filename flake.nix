{
  description = "An example NixOS configuration";
  inputs.home.url = "github:nix-community/home-manager";
  inputs.home.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.npm-build-package.url = "github:serokell/nix-npm-buildpackage";
  inputs.xmonad.url = "github:xmonad/xmonad";
  inputs.xmonad-contrib = {
    url = "github:xmonad/xmonad-contrib";
    inputs.xmonad.follows = "xmonad";
  };

  outputs = { self, home, nixpkgs, npm-build-package, xmonad, xmonad-contrib }:
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
            ./modules/containerd-shim-hack.nix
            home.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.d4hines = { ... }: {
                imports = [ ./modules/home ./modules/home/nixos-only.nix ];
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
    };
}

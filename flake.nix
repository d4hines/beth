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
    let home = ((import ./home.nix)
      { inherit nixpkgs npm-build-package xmonad xmonad-contrib; }
    );
    in
    {
      nixosConfigurations = {
        RADAH = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ({ pkgs, ... }: {
              nixpkgs.overlays =
                [ npm-build-package.overlay xmonad.overlay xmonad-contrib.overlay ];
            })
            ./configuration.nix
            (home.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.d4hines = home;
              })
          ];
        };
      };
    };
}

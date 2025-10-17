{
  description = "An example NixOS configuration";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-filter.url = "github:numtide/nix-filter";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.darwin.follows = "darwin";
      inputs.home-manager.follows = "home-manager";
    };
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
  };
  outputs =
    {
      self,
      home-manager,
      nixpkgs,
      deploy-rs,
      disko,
      nix-filter,
      darwin,
      ...
    }:
    let
      rev = if self ? rev then self.shortRev else "${self.dirtyShortRev}";

      all-overlays = [
        nix-filter.overlays.default
        deploy-rs.overlay
      ] ++ import ./overlays;
      aarch64-linuxPkgs = import nixpkgs {
        system = "aarch64-linux";
        overlays = all-overlays;
      };
      _x86_64Pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = all-overlays;
      };
      CHARLIEPC = import ./machines/CHARLIEPC {
        inherit
          all-overlays
          rev
          disko
          ;
        nixosModules = self.nixosModules;
        home = home-manager;
      };
      UTM = (import ./machines/UTM) {
        inherit
          all-overlays
          rev
          disko
          ;
        nixosModules = self.nixosModules;
        home = home-manager;
      };
    in
    {
      nixosConfigurations = {
        # UTM VM
        UTM = nixpkgs.lib.nixosSystem UTM;
        CHARLIEPC = nixpkgs.lib.nixosSystem CHARLIEPC;
      };
      darwinConfigurations =
        let
          makeDarwin =
            darwin-config: username:
            darwin.lib.darwinSystem {
              system = "aarch64-darwin";
              modules = [
                (
                  { ... }:
                  {
                    nixpkgs = {
                      overlays = all-overlays;
                      config.allowUnfree = true;
                    };
                  }
                )
                darwin-config
                home-manager.darwinModules.home-manager
                {
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users."${username}" = import ./modules/darwin/home.nix {
                    beth-home = self.nixosModules.home {
                      gitUserName = "Daniel Hines";
                      gitUserEmail = "d4hines@gmail.com";
                    };
                  };
                }
              ];
            };
        in
        {
          malak = makeDarwin ./machines/MALAK "dhines";
          yachal = makeDarwin ./machines/YACHAL "d4hines";
        };
      nixosModules = import ./modules;
      homeConfigurations = {
        default = home-manager.lib.homeManagerConfiguration {
          pkgs = aarch64-linuxPkgs;
          modules = [
            ({
              home = {
                username = "d4hines";
                homeDirectory = "/home/d4hines";
                stateVersion = "23.05";
              };
            })
            (import ./modules/home {
              gitUserName = "Daniel Hines";
              gitUserEmail = "d4hines@gmail.com";
            })
          ];
        };
      };
      overlays = {
        default = nixpkgs.lib.composeManyExtensions (import ./overlays);
      };
      deploy.nodes = {
        CHARLIEPC = {
          hostname = "CHARLIEPC";
          profiles.system = {
            user = "root";
            sshUser = "charlie";
            path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.CHARLIEPC;
            remoteBuild = true;
          };
        };
      };

      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;
      formatter.aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.nixfmt-rfc-style;
    };
}

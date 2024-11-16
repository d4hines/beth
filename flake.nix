{
  description = "An example NixOS configuration";
  inputs = {
    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-filter.url = "github:numtide/nix-filter";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { self
    , home
    , nixpkgs
    , deploy-rs
    , nix-filter
    , agenix
    ,
    }:
    let
      rev =
        if self ? rev
        then self.rev
        else "dirty";

      fix-nixpkgs-path = import ./modules/fix-nixpkgs-path.nix { inherit nixpkgs; };
      all-overlays =
        [
          nix-filter.overlays.default
          deploy-rs.overlay
        ]
        ++ import ./overlays;
      aarch64-linuxPkgs = import nixpkgs {
        system = "aarch64-linux";
        overlays =
          all-overlays;
      };
      x86_64Pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = all-overlays;
      };
      packages = {
        x86_64-linux.toolbox = x86_64Pkgs.toolbox;
        aarch64-linux.toolbox = aarch64-linuxPkgs.toolbox;
      };

      MALAK2 = (import ./machines/MALAK2) {
        inherit all-overlays home fix-nixpkgs-path rev;
        nixosModules = self.nixosModules;
      };
      EZRA = (import ./machines/EZRA) {
        inherit all-overlays fix-nixpkgs-path rev agenix;
        nixosModules = self.nixosModules;
      };
    in
    {
      inherit packages;
      nixosConfigurations = {
        # My desktop-on-a-thumb-drive
        MALAK2 = nixpkgs.lib.nixosSystem MALAK2;
        # Server
        EZRA = nixpkgs.lib.nixosSystem EZRA;
      };
      nixosModules = import ./modules;
      deploy.nodes.EZRA = {
        hostname = "ezra.hines.house";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.EZRA;
          remoteBuild = true;
        };
      };
      overlays = {
        default = nixpkgs.lib.composeManyExtension (import ./overlays);
      };
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
      formatter.aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.alejandra;
    };
}

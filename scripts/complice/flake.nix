{
  description = "Exercism Workspace";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    ghcide-nix = {
      url = "github:cachix/ghcide-nix";
      flake = false;
    };
    nixpkgs-mozilla = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ghcide-nix, nixpkgs-mozilla, nix-npm-buildpackage }:
    let
      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
    in {
      overlay = final: prev:
        with prev.lib;
        let
          overlays = [
            nix-npm-buildpackage.overlay
            (import nixpkgs-mozilla)
            (final: prev: { nodejs = prev.nodejs-14_x; })
          ];
        in foldl' (flip extends) (_: prev) overlays final;

      packages.x86_64-linux = let
        pkgs = pkgsFor "x86_64-linux";
        ghcides = import ghcide-nix { };
      in {
        inherit (pkgs) nodejs;
        inherit (ghcides) ghcide-ghc8102 ghcide-ghc884 ghcide-ghc865;
      };

      devShell.x86_64-linux = let
        pkgs = pkgsFor "x86_64-linux";
        setupBin = pkgs.writeShellScriptBin "setup-exercism" ''
          exercism configure --workspace=$PWD --token=''${1?please provide token to use with exercism}
        '';
      in pkgs.mkShell {
        nativeBuildInputs = [
          setupBin

          pkgs.exercism
          pkgs.nixfmt
        ];
      };
    };
}
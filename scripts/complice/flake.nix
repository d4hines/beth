let shell = { nodeVersion ? "14_x"
, nixpkgs ? (import ../. {}).pkgs
}:

with nixpkgs;

let
  nodejs = pkgs."nodejs-${nodeVersion}";

  nodeEnv = buildNpmPackage {
    src = ./.support/env;
  };

  npmWrapper = writeShellScriptBin "npm" ''
    [ -f node_modules ] || ln -fs ${nodeEnv}/node_modules
    main() {
      case "$*" in
        install*)
          exit 0
        ;;
        run*)
          shift
          main "$@"
        ;;
        test*)
          shift
          exec ${nodeEnv}/node_modules/.bin/jest "$@"
        ;;
        lint*)
          exec ${nodeEnv}/node_modules/.bin/eslint . --ext .jsx,.js
        ;;
        *)
          exec ${nodeEnv}/bin/npm "$@"
        ;;
      esac
    }
    main "$@"
  '';

  lintBin = writeShellScriptBin "lint" ''
    ln -fs ${nodeEnv}/node_modules
  '';
in mkShell {
  buildInputs = [
    npmWrapper

    nodejs
    nodePackages.eslint_d
    nodePackages.indium
    nodePackages.prettier
    nodePackages.typescript-language-server
  ];

  shellHook = ''
    export PATH="$PATH:${nodeEnv}/node_modules/.bin"
  '';
};
in
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
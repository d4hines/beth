{ nodeVersion ? "14_x"
, nixpkgs ? (import ./. {}).pkgs
}:

with nixpkgs;

let
  nodejs = pkgs."nodejs-${nodeVersion}";

  nodeEnv = buildNpmPackage {
    src = ./.;
  };

  nodeWrapper = writeShellScriptBin "node" ''
    [ -f node_modules ] || ln -fs ${nodeEnv}/node_modules
    node "$@"
  '';
in
mkShell {
  buildInputs = [
    nodejs
    nodeWrapper
    hello
  ];

  shellHook = ''
    export PATH="$PATH:${nodeEnv}/node_modules/.bin"
    [ -f node_modules ] || ln -fs ${nodeEnv}/node_modules
  '';
}

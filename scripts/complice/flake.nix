{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nix-npm-buildpackage, flake-utils }:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "Complice script ";
      overlay = final: prev:
        with prev.lib;
        let
          overlays = [
            nix-npm-buildpackage.overlay
          ];
        in
          foldl' (flip extends) (_: prev) overlays final;
      shell = { pkgs ? import <nixpkgs> }:
        let
          bp = pkgs.callPackage nix-npm-buildpackage {};
          nodeEnv =
            bp.buildNpmPackage { src = ./.; };
        in
          pkgs.mkShell {
            buildInputs = [ pkgs.nodejs ];
            shellHook = ''
              export PATH="$PATH:${nodeEnv}/node_modules/.bin"
              export NODE_PATH="${nodeEnv}/node_modules"
            '';
          };
    };
}

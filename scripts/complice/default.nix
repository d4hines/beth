{ system ? builtins.currentSystem }:

let
  flake = (import (fetchTarball {
    url =
      "https://github.com/edolstra/flake-compat/tarball/94cf59784c73ecec461eaa291918eff0bfb538ac";
    sha256 = "03qz7h3hpb7jhlyidd4jxxncns40kvw8aqsq5l8d0rsdq9nj02qj";
  }) { src = ./.; }).defaultNix;
in {
  inherit (flake) inputs overlay;
  pkgs = import flake.inputs.nixpkgs { overlays = [ flake.overlay ]; };
}

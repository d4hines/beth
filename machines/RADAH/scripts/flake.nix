{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, dream2nix }:
    let d2n = dream2nix.lib.init {
      systems = [ "x86_64-linux" ];
      config.projectRoot = ./.;
    };
    in
    d2n.makeFlakeOutputs {
      source = ./.;
    };
}

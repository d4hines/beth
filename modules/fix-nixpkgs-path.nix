{nixpkgs}: {
  pkgs,
  lib,
  ...
}: let
  nixpkgsPath = "/etc/nixpkgs/channels/nixpkgs";
in {
  nix = {
    registry.nixpkgs.flake = nixpkgs;
    nixPath = [
      "nixpkgs=${nixpkgsPath}"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
  };

  systemd.tmpfiles.rules = [
    "L+ ${nixpkgsPath}     - - - - ${nixpkgs}"
  ];
}

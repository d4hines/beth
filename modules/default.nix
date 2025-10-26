{
  home = import ./home;
  avahi = import ./avahi.nix;
  fix-nixpkgs-path = import ./fix-nixpkgs-path.nix;
  nixos-home = import ./nixos-home.nix;
  node-exporter = import ./node-exporter.nix;
  promtail = import ./promtail.nix;
  sound = import ./sound.nix;
  twitch = import ./twitch.nix;
  utm = import ./utm.nix;
  graphical = import ./graphical.nix;
  firefox = import ./firefox.nix;
}

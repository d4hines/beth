{
  home = import ./home;
  node-exporter = import ./node-exporter.nix;
  utm = import ./utm.nix;
  graphical = import ./graphical.nix;
  firefox = import ./firefox.nix;
}

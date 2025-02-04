{ beth-home }:
{
  config,
  pkgs,
  ...
}:
{
  imports = [ beth-home ];
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  home.packages = with pkgs; [
    google-chrome
    activate-chrome-tab
    flameshot
    tesseract4
  ];

  programs.home-manager.enable = true;
}

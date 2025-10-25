{ pkgs, ... }:
{
  hardware.opengl.enable = true;
  services.xserver = {
    enable = true;
    displayManager = {
      lightdm.enable = true;
      lightdm.greeters.gtk.enable = true;
      defaultSession = "none+xmonad";
      autoLogin.enable = true;
    };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ../overlays/xmonad/xmonad.hs;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
      ];
    };
  };
  services.xrdp = {
    enable = true;
    defaultWindowManager = "xmonad";
    openFirewall = true; # Or handle firewall manually
  };
  services.xscreensaver.enable = true;
  environment.systemPackages = with pkgs; [
    playerctl
    firefox
    kitty
    gcompris
    haskellPackages.xmobar
    rofi
    (makeDesktopItem {
      name = "ixl";
      desktopName = "IXL";
      exec = "xdg-open https://ixl.com";
      icon = "web-browser";
      categories = [
        "Network"
        "WebBrowser"
      ];
    })
  ];
}

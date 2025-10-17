{ pkgs, ... }:
{
  services.xserver.enable = true;
  # services.xserver = {
  #   enable = true;
  #   layout = "us";
  #   xkbVariant = "";
  #   displayManager.startx.enable = true;
  #   videoDrivers = [ "amdgpu" ];
  # };
  hardware.opengl.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.greeters.gtk.enable = true;
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ../overlays/xmonad/xmonad.hs;
    extraPackages = haskellPackages: [
      haskellPackages.xmonad-contrib
    ];
  };
  services.displayManager.defaultSession = "none+xmonad";
  environment.systemPackages = with pkgs; [
    firefox
    kitty
    gcompris
    haskellPackages.xmobar
    rofi
  ];
}

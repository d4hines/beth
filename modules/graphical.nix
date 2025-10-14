{ pkgs, ... }:
{
  services.xserver.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.greeters.gtk.enable = true;
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad = {
    haskellPackages = pkgs.haskellPackages;
  };
  services.displayManager.defaultSession = "none+xmonad";
  environment.systemPackages = with pkgs; [
    firefox
    kitty
    gcompris
  ];
}

{ pkgs, ... }:
{
  home.packages =
    with pkgs; [
      vscode
      kitty
      nodejs
      obs-studio
      pulseaudio-ctl

      fira-code
      nerdfonts
      dejavu_fonts
    ];

  fonts.fontconfig.enable = true;
  home.file.".xinitrc" = {
    text = ''
      [ -f ~/.xprofile ] && . ~/.xprofile

      # For GNOME keyring 
      dbus-update-activation-environment --systemd DISPLAY
      eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh) export SSH_AUTH_SOCK

      # Fix weird cursor in some GTK apps
      xsetroot -cursor_name left_ptr

      exec ${pkgs.haskellPackages.xmonad}/bin/xmonad
    '';
    executable = true;
  };
  home.file.".xmobarrc".text = (import ./xmobar.nix) { isNixOS = true; };
  services.vscode-server = {
    enable = true;
    useFhsNodeEnvironment = true;
  };
}

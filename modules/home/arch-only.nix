{ pkgs, ... }:
{
  programs.zsh.envExtra = ''
    . $HOME/.nix-profile/etc/profile.d/nix.sh
    export PATH="$PATH:$HOME/.local/bin"
  '';
  home.file.".xinitrc" = {
    text = ''
      [ -f ~/.xprofile ] && . ~/.xprofile

      # needed for VS Code to store secrets correctly
      source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh
      eval $(gnome-keyring-daemon --start)
      export SSH_AUTH_SOCK

      systemctl --user start browser-whitelist
      systemctl --user start twitch-notifications
      systemctl --user start complice-xmobar
      exec ${pkgs.haskellPackages.xmonad}/bin/xmonad
    '';
    executable = true;
  };
  home.file.".xmobarrc".text = (import ./xmobar.nix) { isNixOS = false; };
}

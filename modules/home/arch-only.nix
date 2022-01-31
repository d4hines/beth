{ pkgs, ... }:
{
  programs.zsh.envExtra = ''
    . $HOME/.nix-profile/etc/profile.d/nix.sh
    export PATH="$PATH:$HOME/.local/bin"
  '';
  home.file.".xinitrc" = {
    text = ''
      [ -f ~/.xprofile ] && . ~/.xprofile
      systemctl --user start browser-whitelist
      systemctl --user start twitch-notifications
      systemctl --user start complice-xmobar
      exec ${pkgs.haskellPackages.xmonad}/bin/xmonad
    '';
    executable = true;
  };
  home.file.".xmobarrc".text = (import ./xmobar.nix) { isNixOS = false; };
}

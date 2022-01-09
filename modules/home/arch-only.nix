{ pkgs, ... }: {
  programs.zsh.envExtra = ''
    . $HOME/.nix-profile/etc/profile.d/nix.sh
    export PATH="$PATH:$HOME/.local/bin"
  '';
  home.file.".xinitrc" = {
    text = ''
      [ -f ~/.xprofile ] && . ~/.xprofile
      exec ${pkgs.haskellPackages.xmonad}/bin/xmonad
    '';
    executable = true;
  };
}

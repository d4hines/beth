{ pkgs, ... }: {
  programs.zsh.envExtra = ''
    . $HOME/.nix-profile/etc/profile.d/nix.sh
    export PATH="$PATH:$HOME/.local/bin"
  '';
}

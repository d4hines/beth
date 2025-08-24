{ pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    shellInit = ''
      if [[ -e "$HOME/.zshextra" ]]; then
          source "$HOME/.zshextra"
      fi
    '';
  };
  programs.zoxide.enable = true;
  programs.starship = {
    enable = true;
    presets = [
      "no-nerd-font"
      "jetpack"
    ];
  };
  environment.systemPackages = with pkgs; [
    curl
    git
    vim
    fzf
    # bun
    # python3
    # rsync
    # jq
    # time
    # ripgrep
    # tmux
    # unzip
    # zip
    # cloc
    # wget
    # lsof
    # socat
  ];
}

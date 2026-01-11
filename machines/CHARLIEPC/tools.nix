{ pkgs, ... }:
{
  programs.zsh.enable = true;
  programs.zoxide.enable = true;
  environment.systemPackages = with pkgs; [
    curl
    git
    vim
    fzf
    neofetch
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
  services.transmission = {
    enable = true;
    settings = {
      watch-dir-enabled = true;
    };
  };
}

{ pkgs, ... }:
{

  programs.zsh = {
    enable = true;
    initContent = ''
      if [[ -e "$HOME/.zshextra" ]]; then
          source "$HOME/.zshextra"
      fi
    '';
    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
    };
  };
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
  home.file.".config/kitty/kitty.conf".text = ''
    font_family      Fira Code
    bold_font        Fira Code Bold
    italic_font      auto
    bold_italic_font auto

    copy_on_select yes

    font_size 10

    enabled_layouts tall:bias=50;full_size=1;mirrored=false

    macos_option_as_alt yes
    macos_hide_titlebar yes

    map ctrl+shift+r discard_event

    ${builtins.readFile ../../modules/home/catpuccin.conf}
  '';
  home.file.".tmux.conf".text = builtins.readFile ../../modules/home/tmux.conf;
}

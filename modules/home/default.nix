{pkgs, ...}: {
  home.enableNixpkgsReleaseCheck = true;
  home.packages = with pkgs; [
    nix-tree
    patdiff
    yarn
    yubikey-manager
    age-plugin-yubikey
    log-hours
    go
    gopls
    ffmpeg
    git-lfs
    bun
    zip
    unzip
    dust
    fira-code
    fira-code-nerdfont
    dejavu_fonts
    nil
    alejandra
    haskellPackages.nix-derivation
    cloudflared
  ];
  home.file.".gitconfig".text = builtins.readFile ./.gitconfig; # FIXME:
  programs.home-manager.enable = true;
  # I use Zsh for my shell but it's good to have bash around
  programs.bash.enable = true;
  programs.zsh.enable = true;
  home.file.".config/kitty/kitty.conf".text = ''
    font_family      Fira Code
    bold_font        Fira Code Bold
    italic_font      auto
    bold_italic_font auto

    copy_on_select yes

    font_size 10

    enabled_layouts tall:bias=50;full_size=1;mirrored=false

    shell ${pkgs.toolbox}/bin/toolbox

    macos_option_as_alt yes
    macos_hide_titlebar yes

    map ctrl+shift+r discard_event

    ${builtins.readFile ./catpuccin.conf}
  '';
  fonts.fontconfig.enable = true;
}

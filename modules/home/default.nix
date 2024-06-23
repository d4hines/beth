{pkgs, ...}: let
  theme = import ./theme.nix;
in {
  home.enableNixpkgsReleaseCheck = true;
  home.stateVersion = "21.11";
  home.packages = with pkgs; [
    nix-tree
    patdiff
    yarn
    #openssh
    neofetch
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

    fira-code
    fira-code-nerdfont
    dejavu_fonts

    nil
    alejandra
    haskellPackages.nix-derivation

    lua-language-server

    cloudflared
  ];

  home.file.".ssh/config" = {
    text = ''
      Host arcturus
        HostName ssh.hines.house
        Port 7847
      Host localhost
        UserKnownHostsFile /dev/null
      Host ezra.hines.house
        ProxyCommand ${pkgs.cloudflared}/bin/cloudflared access ssh --hostname %h
    '';
  };
  home.file.".gitconfig".text = builtins.readFile ./.gitconfig;

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
  home.file."lock-screen.png".source = ./lock-screen.png;
  # home.file.".config/lazygit/config.yml".text = builtins.readFile ./lazy_git_config.yml;
  # home.file.".config/helix/config.toml".text = builtins.readFile ./helix_config.yml;
  home.file.".config/discord/settings.json".text = ''    {
        "SKIP_HOST_UPDATE": true,
        "BACKGROUND_COLOR": "#202225",
        "IS_MAXIMIZED": false,
        "IS_MINIMIZED": false,
        "WINDOW_BOUNDS": {
          "x": 2,
          "y": 2,
          "width": 2556,
          "height": 1408
        }
      }'';
}

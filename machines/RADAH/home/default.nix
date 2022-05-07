{ pkgs, cfg, ... }:
let
  homeDirectory = "/home/d4hines";
  username = "d4hines";
  theme = import ./theme.nix;
in
{
  home.packages =
    with pkgs; [
      binutils
      yarn
      openssh
      perf-tools
      jq
      watchexec
      cloc
      wget
      docker-compose
      fzf
      git-crypt
      file
      bat
      lsof
      bubblewrap
      psmisc
      gdb
      comby
      chkrootkit
      socat
      shellcheck
      gh-stack
      deploy-rs.deploy-rs
      time

      playerctl
      pavucontrol
      xclip
      signal-desktop
      dmenu
      graphviz
      my-google-chrome
      haskellPackages.xmobar
      haskellPackages.xmonad
      picom
      mailspring
      parted
      activate-chrome-tab
      preview
      mgba
      exercism
      inkscape

      rnix-lsp
      nixfmt
      haskellPackages.nix-derivation

      zoom
      zotero
    ];

  home.file.".ssh/id_rsa" = {
    text = builtins.readFile ../../../secrets/id_rsa;
    onChange = "sudo chmod 700 ~/.ssh/id_rsa";
  };
  home.file.".ssh/id_rsa.pub" = {
    text = builtins.readFile ../../../keys/id_rsa.pub;
    onChange = "sudo chmod 644 ~/.ssh/id_rsa.pub";
  };
  home.file.".ssh/authorized_keys" = {
    text = builtins.readFile ../../../keys/authorized_keys;
    onChange = "sudo chmod 600 ~/.ssh/authorized_keys";
  };

  home.sessionVariables = theme // {
    BROWSER = "chrome";
    EDITOR = "vim";
    DEFAULT_USER = username; # for agnoster oh-my-zsh theme.
    TEZOS_DIR = "${homeDirectory}/repos/tezos";
    OCAMLRUNPARAM = "b";
    GHSTACK_OAUTH_TOKEN = builtins.readFile ../../../secrets/gh_token;
  };

  programs.home-manager.enable = true;
  # I use Zsh for my shell but it's good to have bash around
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.zsh.initExtra =
    # Random Tezos thing
    ''
        create_mockup () {
        	if [[ ! -d /tmp/mockup ]]; then
        		tezos-client \
        		  --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
        		  --base-dir /tmp/mockup \
        		  --mode mockup \
        		  create mockup
        	fi
      } 
      #TODO: why is this necessary when I already set??
      export EDITOR=vim

      # TODO: seems like home.sessionPath shoudl work but doesn't??
      export PATH=~/.npm-global/bin:$PATH
    '';
  programs.zsh.shellAliases = {
    # Only requires flakes-enabled nix and for this repo
    # to be at path ~/repos/beth. (i.e works even if
    # home-hanager isn't installed yet.)
    # You can install nix with the nix-flakes-installer, e.g:
    # sh <(curl -L https://github.com/numtide/nix-flakes-installer/releases/download/nix-2.4pre20210604_8e6ee1b/install)

    icat = "kitty +kitten icat";
    fzf_preview = ''fzf --preview "preview {}" --preview-window left:40%'';
    watchexec = "watchexec --shell='bash --login -O expand_aliases'";
    scu = "systemctl --user";
    # Tezos specific stuff
    cdp = "cd $TEZOS_DIR/src/proto_alpha/lib_protocol";
    cdt = "cd $TEZOS_DIR";
    cdu = "cd $TEZOS_DIR/src/proto_alpha/lib_protocol/test/unit";
    turn_off_warnings = ''export OCAMLPARAM="_,w=-27-26-32-33-20-21-37-34"'';
    runtest = "dune build --terminal-persistence=clear-on-rebuild  @runtest_proto_alpha --watch";
    test_globals = ''(cdu && dune build @runtest --force ) && dune exec ./src/proto_alpha/lib_protocol/test/main.exe -- test "global table of constants" -c && tezt global_constant'';
    dbw = "dune build --terminal-persistence=clear-on-rebuild --watch";
    tezt = ''dune exec tezt/tests/main.exe --'';
    destroy_mockup = "rm -rf /tmp/mockup";
    mockup_client = "create_mockup && tezos-client --mode mockup --base-dir /tmp/mockup";
    client = "mockup_client";
  };
  programs.zsh.oh-my-zsh.enable = true;
  programs.zsh.oh-my-zsh.theme = "agnoster";

  home.file.".config/kitty/kitty.conf".text = with theme; "
    font_family      Fira Code
    bold_font        Fira Code Bold
    italic_font      auto
    bold_italic_font auto

    font_size 15

    background            ${GREY_COLOR}
    foreground            #eaeaea
    cursor                ${LIME_COLOR}
    selection_background  #9096a0
    color0                #181818
    color8                #181818
    color1                #bf081d
    color9                #bf081d
    color2                #3d9751
    color10               #3d9751
    color3                ${GOLD_COLOR}
    color11               ${GOLD_COLOR}
    color4                ${CARET_COLOR}
    color12               ${CARET_COLOR}
    color5                ${PURPLE_COLOR}
    color13               ${PURPLE_COLOR}
    color6                ${CYAN_COLOR}
    color14               ${CYAN_COLOR}
    color7                #ffffff
    color15               #ffffff
    selection_foreground  #252b35
    ";

  # Run on interactive shells
  programs.direnv.enable = true;
  programs.direnv.enableZshIntegration = true;
  programs.direnv.nix-direnv.enable = true;

  programs.git = {
    enable = true;
    userEmail = "d4hines@gmail.com";
    userName = "Daniel Hines";

    extraConfig = {
      pull.rebase = false;
    };

    aliases = {
      co = "checkout";
      branchname = "symbolic-ref --short -q HEAD";
      cp = "cherry-pick";
    };
  };
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;
  services.gpg-agent.enableScDaemon = true;
  services.gpg-agent.enableSshSupport = true;
  services.gpg-agent.defaultCacheTtl = 60;
  services.gpg-agent.maxCacheTtl = 120;
  services.gpg-agent.sshKeys = [ "0x26D64B46D60FE2BB" ];

  # for Pause/Play
  services.playerctld.enable = true;

  programs.htop.enable = true;
  programs.man.enable = true;
  services.dunst.enable = true;
  services.dunst.settings = with theme; {
    global = {
      font = "DejaVu Sans 12";
      geometry = "0x0-30+20";
      transparency = 0;
      padding = 12;
      horizontal_padding = 12;
      foreground = "#ffffff";
      frame_width = 3;
      frame_color = "#56b6c2";
      markup = "full";
      format = ''<b>%s</b>\n%b'';
      max_icon_size = 48;
      corner_radius = 5;
    };
    urgency_low = {
      background = PLAIN_COLOR;
      timeout = 0;
    };
    urgency_normal = {
      background = "#252b35";
      foreground = "#ffffff";
      timeout = 0;
    };
    urgency_critical = {
      background = PINK_COLOR;
      foreground = "#ffffff";
      timeout = 0;
    };
  };

  services.redshift = {
    enable = true;
    latitude = 36.8;
    longitude = -76.0;
  };

  services.dropbox.enable = true;
  services.flameshot.enable = true;
  systemd.user.services = with pkgs; {
    twitch-notifications = twitch-notifications-service;
    browser-whitelist = browser-whitelist-service;
    # grayscale = grayscale-service;
  };
  home.file.".config/exercism/user.json".text = ''
    {
     "apibaseurl": "https://api.exercism.io/v1",
     "token": "${builtins.readFile ../../../secrets/exercism}",
     "workspace": "/home/d4hines/repos/exercism"
     }'';
}

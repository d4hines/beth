{ pkgs, cfg, ... }:
let
  homeDirectory = "/home/d4hines";
  username = "d4hines";
  theme = import ./theme.nix;
in
{
  home.packages =
    with pkgs; [
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
      activate-chrome-tab
      psmisc
      gdb

      ligo
      poetry

      playerctl
      xclip
      signal-desktop
      dmenu
      graphviz
      my-google-chrome
      haskellPackages.xmobar
      haskellPackages.xmonad
      picom
      flameshot
      mailspring
      parted
      complice-xmobar-server

      rnix-lsp
      nixpkgs-fmt

      zoom
      zotero
    ];

  home.file.".ssh/id_rsa" = {
    text = builtins.readFile ../../secrets/id_rsa;
    onChange = "sudo chmod 700 ~/.ssh/id_rsa";
  };
  home.file.".ssh/id_rsa.pub" = {
    text = builtins.readFile ../../keys/id_rsa.pub;
    onChange = "sudo chmod 644 ~/.ssh/id_rsa.pub";
  };
  home.file.".ssh/authorized_keys" = {
    text = builtins.readFile ../../keys/authorized_keys;
    onChange = "sudo chmod 600 ~/.ssh/authorized_keys";
  };

  home.sessionVariables = theme // {
    BROWSER = "chrome";
    EDITOR = "vim";
    DEFAULT_USER = username; # for agnoster oh-my-zsh theme.
    TEZOS_DIR = "${homeDirectory}/repos/tezos";
    OCAMLRUNPARAM = "b";
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
    # Arbitrary scripts can be run on specific filters.
    # See https://dunst-project.org/documentation/#RULES
    play_sound = {
      # For some reason this isn't working anymore
      # It was originally working in this commit: 80b8a268771370b12e42f3d04958d6399f187eca
      appname = "notify-send";
      # Plays the system bell sound.
      script = ''echo -n "\a"'';
    };
  };

  services.redshift.enable = true;
  services.redshift.latitude = 36.8508;
  services.redshift.longitude = 76.2859;

  services.dropbox.enable = true;

  home.file.".xprofile".text = ''
    ${pkgs.complice-xmobar-server}/bin/complice-xmobar &
    ${pkgs.twitch-notifications}/bin/twitch-notifications &
    dunst &
    flameshot &
  '';
}

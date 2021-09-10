{
  inputs.home.url = "github:nix-community/home-manager";
  inputs.home.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.npm-build-package.url = "github:serokell/nix-npm-buildpackage";
  outputs = { self, home, nixpkgs, npm-build-package }:
    let
      homeDirectory = "/home/d4hines";
      username = "d4hines";
      system = "x86_64-linux";
    in
      {
        homeConfigurations.d4hines = home.lib.homeManagerConfiguration {
          inherit homeDirectory username system;
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ npm-build-package.overlay ] ++ (import ./overlays);
          };
          configuration = { pkgs, ... }:
            let
              # Theme adapted with thanks from https://github.com/azemoh/vscode-one-monokai
              theme =
                {
                  PLAIN_COLOR = "#abb2bf";
                  GREY_COLOR = "#282c34";
                  DARK_GREY_COLOR = "#21252B";
                  CARET_COLOR = "#528bff";
                  PINK_COLOR = "#e06c75";
                  CYAN_COLOR = "#56b6c2";
                  LIME_COLOR = "#98c379";
                  PURPLE_COLOR = "#c678dd";
                  BROWN_COLOR = "#d19a66";
                  GOLD_COLOR = "#e5c07b";
                  BLUE_COLOR = "#61afef";
                  COMMENT_COLOR = "#5c6370";
                };
            in
              {
                nixpkgs.overlays = (import ./overlays);
                nixpkgs.config.allowUnfree = true;
                home.stateVersion = "20.09";
                home.packages = with pkgs; [
                  # needed for my hacky way of building xmonad
                  stack
                  openssh
                  perf-tools
                  jq
                  neofetch
                  watchexec
                  cloc
                  pandoc
                  my-nodejs

                  playerctl
                  xclip
                  signal-desktop
                  dmenu
                  i3status
                  dunst
                  pastel
                  graphviz
                  my-google-chrome

                  rnix-lsp
                  nixpkgs-fmt

                  zoom
                  zotero

                  fira-code
                  nerdfonts
                  dejavu_fonts
                ];

                home.file.".ssh/id_rsa" = {
                  text = builtins.readFile ./secrets/id_rsa;
                  onChange = "sudo chmod 700 ~/.ssh/id_rsa";
                };
                home.file.".ssh/id_rsa.pub" = {
                  text = builtins.readFile ./keys/id_rsa.pub;
                  onChange = "sudo chmod 644 ~/.ssh/id_rsa.pub";
                };

                fonts.fontconfig.enable = true;

                # https://nix-community.github.io/home-manager/options.html#opt-nixpkgs.config
                # nixpkgs.config = { allowBroken = true; } 

                home.sessionVariables = theme // {
                  BROWSER = "chrome";
                  EDITOR = "vim";
                  COMPLICE_TOKEN = builtins.readFile ./secrets/complice_api;
                  DEFAULT_USER = username; # for agnoster oh-my-zsh theme.
                  TEZOS_DIR = "${homeDirectory}/repos/tezos";
                };

                programs.home-manager.enable = true;
                programs.bash.enable = true;
                programs.zsh.enable = true;
                programs.zsh.initExtra =
                  # Add Nix to the path
                  ''
                    . ~/.nix-profile/etc/profile.d/nix.sh
                  ''
                  + # Random Tezos thing
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
                  ''
                  + # Start the graphical environment
                  # This command needs to come last, as exec will take over the process.
                  # Additionally:
                  # - Start my browsing whitelist script
                  ''
                    if [ "$(tty)" = "/dev/tty1" ]; then
                      exec startx
                    fi
                  '';
                # Add the scripts and dmenu scripts to the path  
                programs.zsh.envExtra = ''
                  export PATH=${homeDirectory}/scripts:${homeDirectory}/scripts/dmenu_scripts:${homeDirectory}/.local/bin:${homeDirectory}/repos/tezos:$PATH
                '';
                programs.zsh.shellAliases = {
                  # Only requires flakes-enabled nix and for this repo
                  # to be at path ~/repos/beth. (i.e works even if
                  # home-hanager isn't installed yet.)
                  # You can install nix with the nix-flakes-installer, e.g:
                  # sh <(curl -L https://github.com/numtide/nix-flakes-installer/releases/download/nix-2.4pre20210604_8e6ee1b/install)
                  home_reload =
                    (
                      let
                        original = "home-manager switch --flake .#d4hines";
                        nix-version = "nix run github:nix-community/home-manager --no-write-lock-file -- switch --flake .#d4hines";
                      in
                        "(cd ~/repos/beth && which home-manager && ${original} || ${nix-version})"
                    );
                  save_config = "(cd ~/repos/beth/aconfmgr && ./aconfmgr save -c ../arch_config)";
                  icat = "kitty +kitten icat";
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
                programs.direnv.nix-direnv.enableFlakes = true;

                programs.emacs = {
                  enable = true;
                  extraPackages = epkgs: [];
                  # extraConfig = builtins.readFile ./emacs.el;
                };


                programs.git = {
                  enable = true;
                  userEmail = "d4hines@gmail.com";
                  userName = "Daniel Hines";

                  extraConfig = {
                    pull.rebase = false;
                  };

                  aliases = {
                    co = "checkout";
                  };
                };
                programs.gpg.enable = true;
                services.gpg-agent.enable = true;
                services.gpg-agent.enableScDaemon = true;
                services.gpg-agent.enableSshSupport = true;
                services.gpg-agent.defaultCacheTtl = 60;
                services.gpg-agent.maxCacheTtl = 120;
                services.gpg-agent.sshKeys = [ "0x26D64B46D60FE2BB" ];

                services.clipmenu.enable = true;

                # for Pause/Play
                services.playerctld.enable = true;

                programs.htop.enable = true;
                home.file."scripts".source = ./scripts;
                home.file."scripts".onChange = "rm -rf ${homeDirectory}/.cache/dmenu_run";
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
                  shortcuts = {
                    close = "ctrl+space";
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

                services.flameshot.enable = true;

                services.redshift.enable = true;
                services.redshift.latitude = 36.8508;
                services.redshift.longitude = 76.2859;

                xsession.enable = true;
                xsession.initExtra = ''
                  xmodmap ~/.Xmodmap
                  export LANG=en_US.UTF-8
                  xmobar &
                  ~/scripts/browser_whitelist.js &
                '';
                xsession.windowManager.command = "xmonad";
                # https://brianbuccola.com/how-to-install-xmonad-and-xmobar-via-stack/
                home.file.".xmonad/xmonad.hs" = {
                  text = builtins.readFile ./xmonad.hs;
                  onChange = "xmonad --recompile && xmonad --restart";
                };
                home.file.".xinitrc" = {
                  # Because home-manager puts a lot of settijngs in .xsession,
                  # all we do in .xinit is call .xsession.
                  text = ''#!/bin/sh
                  . ~/.xsession
                '';
                  executable = true;
                };
                home.file.".xmobarrc".text = with theme; ''
                  Config {
                    font = "xft:Fira Code:size=14:antialias=true:hinting=true:bold,Noto Color Emoji:size=14:antialias=true:hinting=true"
                   , bgColor          = "${DARK_GREY_COLOR}"
                   , fgColor          = "${PLAIN_COLOR}"
                   , position         = BottomSize C 100 24
                   , sepChar          =  "%"   -- delineator between plugin names and straight text
                   , alignSep         = "}{"  -- separator between left-right alignment
                   , template         = " %cpu% | %memory% } %complice% { %date% | %time_norfolk% | %time_paris%"
                   , commands =
                        [ Run Date           "%a, %d %b %Y" "date" 10
                        , Run Date           "%I:%M %p" "time_norfolk" 10
                        , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                        , Run Memory ["-t","Mem: <usedratio>%"] 10
                        , Run Com "${homeDirectory}/scripts/paris_date" ["+%I:%M %p"] "time_paris" 10
                        , Run Com "${homeDirectory}/scripts/complice" [] "complice" 30
                        ]
                   }
                '';
              };
        };
      };
}

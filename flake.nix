{
  inputs.home.url = "github:nix-community/home-manager";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  outputs = { self, home, nixpkgs }:
    {
      homeConfigurations.d4hines = home.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/d4hines";
        username = "d4hines";
        configuration = { config, pkgs, ... }:
          let
            # Adapted with thanks from https://github.com/azemoh/vscode-one-monokai
            theme =
              {
                plain = "#abb2bf";
                background = "#282c34";
                caret = "#528bff";
                pink = "#e06c75";
                cyan = "#56b6c2";
                lime = "#98c379";
                purple = "#c678dd";
                brown = "#d19a66";
                gold = "#e5c07b";
                blue = "#61afef";
                comment = "#5c6370";
              };
          in
            {
              home.stateVersion = "20.09";
              home.packages = with pkgs; [
                openssh
                perf-tools
                jq
                neofetch

                aerc
                signal-desktop
                dmenu
                xmobar
                dunst

                rnix-lsp
                nixpkgs-fmt

                zoom
                zotero

                dejavu_fonts
                fira-code
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

              home.sessionVariables = {
                BROWSER = "brave";
                EDITOR = "vim";
                COMPLICE_TOKEN = builtins.readFile ./secrets/complice_api;
              };

              # v This was apparently required
              programs.home-manager.enable = true;
              programs.bash.enable = true;
              programs.zsh.enable = true;
              programs.zsh.initExtra =
                # Add Nix to the path
                ''
                  . ~/.nix-profile/etc/profile.d/nix.sh
                ''
                + # Load Opam hooks
                ''
                  test -r /home/d4hines/.opam/opam-init/init.sh && . /home/d4hines/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
                ''
                + # Start the graphical environment
                # This command needs to come last, as exec will take over the process.
                # Also start a watch to auto commit and push any changes to notes.
                ''
                  if [ -z "''${DISPLAY}" ] && [ "$(tty)" = "/dev/tty1" ]; then
                    watch -n 10 'cd ~/repos/notes && git add -A && git commit -am "autocommit" || git push && echo "last updated $(date)" > last_updated' &> /dev/null &
                    exec startx
                  fi
                '';
              programs.zsh.shellAliases = {
                # Only requires flakes-enabled nix and for this repo
                # to be at path ~/repos/beth. (i.e works even if
                # home-hanager isn't installed yet.)
                # You can install nix with the nix-flakes-installer, e.g:
                # sh <(curl -L https://github.com/numtide/nix-flakes-installer/releases/download/nix-2.4pre20210604_8e6ee1b/install)
                home_reload = "(cd ~/repos/beth && nix run github:nix-community/home-manager --no-write-lock-file -- switch --flake .#d4hines)";
                save_config = "(cd ~/repos/beth/aconfmgr && ./aconfmgr save -c ../arch_config)";
                icat = "kitty +kitten icat";
              };
              programs.zsh.oh-my-zsh.enable = true;
              programs.zsh.oh-my-zsh.theme = "agnoster";

              home.file.".config/kitty/kitty.conf".text = with theme; "
                font_family      Fira Code
                bold_font        Fira Code Bold
                italic_font      auto
                bold_italic_font auto

                font_size 15

                background            ${background}
                foreground            #eaeaea
                cursor                ${lime}
                selection_background  #9096a0
                color0                #181818
                color8                #181818
                color1                #bf081d
                color9                #bf081d
                color2                #3d9751
                color10               #3d9751
                color3                ${gold}
                color11               ${gold}
                color4                ${caret}
                color12               ${caret}
                color5                ${purple}
                color13               ${purple}
                color6                ${cyan}
                color14               ${cyan}
                color7                #ffffff
                color15               #ffffff
                selection_foreground  #252b35
                ";

              # Run even on non-interactive shells
              # programs.bash.bashrcExtra = ''
              #   . ~/.nix-profile/etc/profile.d/nix.sh 
              #   test -r /home/d4hines/.opam/opam-init/init.sh && . /home/d4hines/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
              #   '';

              # Run on interactive shells
              programs.direnv.enable = true;
              programs.direnv.enableZshIntegration = true;
              programs.direnv.nix-direnv.enable = true;
              programs.direnv.nix-direnv.enableFlakes = true;

              # I use Brave's Sync feature to sync extensions and settings
              # across installations.
              # Extensions I use:
              # - LastPass, password manager
              # - DarkReader, beautiful dark mode for websites.
              # - Zotero Connector, for Zotero integration
              # - BetterTV, to understand what the kids are saying on Twitch
              # - Complice New Tab page, to keep me on track.
              programs.brave.enable = true;

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

              programs.htop.enable = true;

              # TODO: https://nix-community.github.io/home-manager/options.html#opt-programs.kitty.font.package

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
                  background = plain;
                  timeout = 0;
                };
                urgency_normal = {
                  background = background;
                  foreground = "#ffffff";
                  timeout = 0;
                };
                urgency_critical = {
                  background = pink;
                  foreground = "#ffffff";
                  timeout = 0;
                };
              };

              services.flameshot.enable = true;

              services.redshift.enable = true;
              services.redshift.latitude = 36.8508;
              services.redshift.longitude = 76.2859;

              xsession.enable = true;
              xsession.initExtra = ''
                xmodmap ~/.Xmodmap
              '';
              xsession.windowManager.xmonad.enable = true;
              xsession.windowManager.xmonad.enableContribAndExtras = true;
              xsession.windowManager.xmonad.config = ./xmonad.hs;
              home.file.".Xmodmap".text = ''
                clear Lock
                keycode 9 = Caps_Lock NoSymbol Caps_Lock
                keycode 66 = Escape NoSymbol Escape
              '';
              home.file.".xinitrc" = {
                # Because home-manager puts a lot of settijngs in .xsession,
                # all we do in .xinit is call .xsession.
                text = ''#!/bin/sh
                  . ~/.xsession
                '';
                executable = true;
              };
              home.file.".xmobarrc".text = ''
                 Config { font = "xft:Bitstream Vera Sans Mono:size=14:antialias=true"
                , bgColor = "black"
                , fgColor = "grey"
                , position = BottomW R 90
                , commands = [ Run Weather "KNGU" ["-t"," <tempF>F","-L","64","-H","77","--normal","green","--high","red","--low","lightblue"] 36000
                             , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                             , Run Memory ["-t","Mem: <usedratio>%"] 10
                             , Run Swap [] 10
                             , Run Date "%a %b %_d %l:%M" "date" 10
                             , Run StdinReader
                             ]
                , sepChar = "%"
                , alignSep = "}{"
                , template = "%StdinReader% }{ %cpu% | %memory% * %swap%    <fc=#ee9a00>%date%</fc> | %EGPF%"
                }
              '';
            };
      };
    };
}

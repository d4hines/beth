{
  inputs.home.url = "github:nix-community/home-manager";

  outputs = { self, home }:
  {
    homeConfigurations.d4hines = home.lib.homeManagerConfiguration {
      system = "x86_64-linux";
      homeDirectory = "/home/d4hines";
      username = "d4hines";
      configuration = { config, pkgs, ... }: {
        home.stateVersion = "20.09";
        home.packages = with pkgs; [
            
            openssh
            perf-tools
            
            aerc
            signal-desktop
            dmenu
            xclip
            xmobar

            logseq
            zoom
            zotero

            

        ];
        
        # https://nix-community.github.io/home-manager/options.html#opt-nixpkgs.config
        # nixpkgs.config = { allowBroken = true; } 

        # home.keyboard.options = [""]

        # home.sessionVariables = {
        #   FOO = "Hello";
        #   BAR = "${config.home.sessionVariables.FOO} World!";
        # };

        # v This was apparently required
        programs.home-manager.enable = true;
        programs.bash.enable = true;
        # Run even on non-interactive shells
        programs.bash.bashrcExtra = ''
          . ~/.nix-profile/etc/profile.d/nix.sh 
          test -r /home/d4hines/.opam/opam-init/init.sh && . /home/d4hines/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
        '';
        # Run on interactive shells
        programs.bash.initExtra = 
          # Start the graphical environment
          ''
          echo hello
          if [ -z "''${DISPLAY}" ] && [ "$(tty)" = "/dev/tty1" ]; then
            echo "startin x"
            exec startx
          fi
          '';
        programs.bash.shellAliases = {
          # Only requires flakes-enabled nix and for this repo
          # to be at path ~/repos/beth. (i.e works even if
          # home-hanager isn't installed yet.)
          # You can install nix with the nix-flakes-installer, e.g:
          # sh <(curl -L https://github.com/numtide/nix-flakes-installer/releases/download/nix-2.4pre20210604_8e6ee1b/install)
          home_reload = "cd ~/repos/beth && nix run github:nix-community/home-manager --no-write-lock-file -- switch --flake .#d4hines";
        };
        programs.direnv.enable = true;
        programs.direnv.enableBashIntegration = true;
        programs.direnv.nix-direnv.enable = true;
        programs.direnv.nix-direnv.enableFlakes = true;

        programs.chromium = {
            enable = true;
            extensions = [
                # Dark Reader
                { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; }
                # LastPass
                { id = "hdokiejnpimakedhajhdlcegeplioahd"; }
            ];
        }; 

        # programs.gh.enable = true;
        # programs.gh.gitProtocol = "ssh";
        # https://nix-community.github.io/home-manager/options.html#opt-programs.gh.aliases

        programs.git = {
            enable = true;
            userEmail = "d4hines@gmail.com";
            userName = "Daniel Hines";

            extraConfig = {
            pull.rebase = false;
            };

            aliases = {
            # a = "add -p";
            co = "checkout";
            cob = "checkout -b";
            # f = "fetch -p";
            # c = "commit";
            # p = "push";
            # ba = "branch -a";
            # bd = "branch -d";
            # bD = "branch -D";
            # d = "diff";
            # dc = "diff --cached";
            # ds = "diff --staged";
            # r = "restore";
            # rs = "restore --staged";
            # st = "status -sb";

            # reset
            # soft = "reset --soft";
            # hard = "reset --hard";
            # s1ft = "soft HEAD~1";
            # h1rd = "hard HEAD~1";

            # logging
            # lg =
            #     "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
            # plog =
            #     "log --graph --pretty='format:%C(red)%d%C(reset) %C(yellow)%h%C(reset) %ar %C(green)%aN%C(reset) %s'";
            # tlog =
            #     "log --stat --since='1 Day Ago' --graph --pretty=oneline --abbrev-commit --date=relative";
            # rank = "shortlog -sn --no-merges";

            # delete merged branches
            # bdm = "!git branch --merged | grep -v '*' | xargs -n 1 git branch -d";
            };
        };
        programs.gpg.enable = true;
        programs.htop.enable = true;

        programs.kitty.enable = true;
        # TODO: https://nix-community.github.io/home-manager/options.html#opt-programs.kitty.font.package

        programs.man.enable = true;
        services.dunst.enable = true;
        services.flameshot.enable = true;

        services.redshift.enable = true;
        services.redshift.latitude = 36.8508;
        services.redshift.longitude = 76.2859;

        xsession.enable = true;
        xsession.initExtra =  "xmodmap ~/.Xmodmap";
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
        home.file.".xmobarrc".text = builtins.readFile ./xmobarrc;
        
      };
    };
  };
}

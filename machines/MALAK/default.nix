{
  rev,
  pkgs,
}: let
  theme = import ../../modules/home/theme.nix;
in [
  ../../modules/twitch.nix
  ({pkgs, ...}: {
    nixpkgs = {
      config = {
        allowUnfree = true;
        allowUnfreePredicate = _: true;
      };
    };
  })
  {
    home.stateVersion = "21.11";
    home.homeDirectory = "/home/d4hines";
    home.username = "d4hines";
    nixpkgs.config.allowUnfree = true;
    home.shellAliases = {
      startx = "exec startx"; # ensures logout after x ends
    };

    home.file.".zshextra".text = ''
      # TODO: this is broken for now
      # export GPG_TTY="$(tty)"
      # export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      # gpg-connect-agent updatestartuptty /bye > /dev/null

      if [[ -f "$HOME/.cargo/env" ]]; then
        source "$HOME/.cargo/env"
      fi
      alias yggit=$HOME/repos/yggit/target/debug/yggit
    '';

    home.file.".config/chrome-flags.conf".text = "--remote-debugging-port=9222";
    home.file.".config/chromium-flags.conf".text = "--remote-debugging-port=9222";

    # This isn't working for now
    # home.file.".gnupg/gpg-agent.conf".text = ''
    #   # https://github.com/drduh/config/blob/master/gpg-agent.conf
    #   # https://www.gnupg.org/documentation/manuals/gnupg/Agent-Options.html
    #   enable-ssh-support
    #   ttyname $GPG_TTY
    #   default-cache-ttl 60
    #   max-cache-ttl 120
    #   pinentry-program /usr/bin/pinentry-gnome3
    # '';

    home.packages = with pkgs; [
      toolbox
      #signal-desktop

      vscode-wayland # depends on fedora install for now!
      #google-chrome
      chromium
      #vscodium
      dmenu
      haskellPackages.xmonad
      haskellPackages.xmobar
      xorg.xev
      xorg.xset
      #activate-chrome-tab
      pulseaudio-ctl
      brightnessctl
      #zoom
      # discord
      #yubikey-manager-qt
      noto-fonts-emoji
      deploy-rs.deploy-rs
      #signal-cli
    ];
    # for Pause/Play
    services.playerctld.enable = true;
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
    #services.twitch-notifications.enable = true;

    services.redshift = {
      enable = true;
      latitude = 36.8;
      longitude = -76.0;
    };
    fonts.fontconfig.enable = true;

    # services.dropbox.enable = true;
    # services.flameshot.enable = true;
    home.file.".Xresources_ore".text = ''
      Xft.dpi: 192
    '';
    home.file.".xinitrc" = {
      text = ''
        [ -f ~/.xprofile ] && . ~/.xprofile

        flameshot &

        # For GNOME keyring
        dbus-update-activation-environment --systemd DISPLAY
        eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh) export SSH_AUTH_SOCK

        # Fix weird cursor in some GTK apps
        xsetroot -cursor_name left_ptr

        # Set lower key repeat delay and higher repeat rate
        xset r rate 200 50

        if xrandr | grep -q "HDMI-A-0 connected"; then
          xrandr --output eDP --off --output HDMI-A-0 --primary
        fi
        setxkbmap -option "caps:swapescape,altwin:swap_alt_win"
        xrdb -merge ~/.Xresources_ore
        #if lsmod | grep -q "thinkpad"; then xrdb -merge ~/.Xresources_ore
          # swap caps and escape on the internal keyboard of ORE
          #setxkbmap -device $(xinput list | grep 'AT Translated Set 2 keyboard' | grep -o 'id=[0-9]*' | grep -o '[0-9]*') -option "caps:swapescape"
        #fi

        exec ${pkgs.haskellPackages.xmonad}/bin/xmonad
      '';
      executable = true;
    };
    home.file.".xmobarrc".text = import ../../modules/home/xmobar.nix;
    home.file.".config/revision".text = "${rev}";
  }
  ../../modules/home
]

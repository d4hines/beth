{
  rev,
  pkgs,
}: let
  theme = import ../../modules/home/theme.nix;
in [
  {
    home.stateVersion = "21.11";
    home.homeDirectory = "/home/d4hines";
    home.username = "d4hines";
    nixpkgs.config.allowUnfree = true;
    home.shellAliases = {
     startx = "exec startx"; # ensures logout after x ends
    };

    home.file.".zshextra".text = ''
      export GPG_TTY="$(tty)"
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      gpg-connect-agent updatestartuptty /bye > /dev/null
      PATH=$PATH:$HOME/.cargo/bin
      source $HOME/.cargo/env
    '';

    home.packages = with pkgs; [
      toolbox
      #i3lock
      signal-desktop
      # google-chrome
      dmenu
      haskellPackages.xmonad
      haskellPackages.xmobar
      activate-chrome-tab
      #kitty
      pulseaudio-ctl
      zoom
      #discord
      yubikey-manager-qt
      vlc
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

    services.redshift = {
      enable = true;
      latitude = 36.8;
      longitude = -76.0;
    };

    #services.dropbox.enable = true;
    services.flameshot.enable = true;
    home.file.".xinitrc" = {
      text = ''
        [ -f ~/.xprofile ] && . ~/.xprofile

        # For GNOME keyring
        dbus-update-activation-environment --systemd DISPLAY
        eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh) export SSH_AUTH_SOCK

        # Fix weird cursor in some GTK apps
        xsetroot -cursor_name left_ptr

        # Set lower key repeat delay and higher repeat rate
        xset r rate 200 50

        # for laptop keyboard
        setxkbmap -option "caps:swapescape"

        xrdb -merge ~/.Xresources

        exec ${pkgs.haskellPackages.xmonad}/bin/xmonad
      '';
      executable = true;
    };
    home.file.".xmobarrc".text = import ./xmobar.nix;
    programs.obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [obs-command-source];
    };
    home.file.".config/revision".text = "${rev}";
    home.file."lock-screen.png".source = ./lock-screen.png;
  }
  ../../modules/home
]

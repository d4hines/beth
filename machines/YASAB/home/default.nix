{
  pkgs,
  cfg,
  ...
}: let
  homeDirectory = "/home/d4hines";
  username = "d4hines";
  theme = import ../../../modules/home/theme.nix;
in {
  home.packages = with pkgs; [
    toolbox
    gdb
    linuxKernel.packages.linux_5_15.perf
    bubblewrap
    psmisc
    chkrootkit
    deploy-rs.deploy-rs
    gnome3.adwaita-icon-theme
    playerctl
    pavucontrol
    xclip
    dmenu
    graphviz
    google-chrome
    haskellPackages.xmobar
    haskellPackages.xmonad
    picom
    parted
    activate-chrome-tab
    exercism
    vscode
    kitty
    pulseaudio-ctl
    zoom
    zotero
    discord
    yubikey-manager-qt
    mpv
    log-hours
    brightnessctl
    gnome.nautilus
    libnotify
    pulseaudioFull

    alejandra
    haskellPackages.nix-derivation
  ];

  home.file.".zshextra".text = ''
    if [[ -f "$HOME/.cargo/env" ]]; then
      source "$HOME/.cargo/env"
    fi
    alias yggit=$HOME/repos/yggit/target/debug/yggit

    #### Zlong alert ####
    # Plays an alert for long-running commands
    DONE_WAV=${../../../modules/home/done.wav}
    ${builtins.readFile ../../../overlays/scripts/zlong_alert.zsh}
    #####################
  '';

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

  # This isn't working for now
  # programs.gpg.enable = true;
  # services.gpg-agent.enable = true;
  # services.gpg-agent.enableScDaemon = true;
  # services.gpg-agent.enableSshSupport = true;
  # services.gpg-agent.defaultCacheTtl = 60;
  # services.gpg-agent.maxCacheTtl = 120;
  # services.gpg-agent.sshKeys = ["0x26D64B46D60FE2BB"];
  # services.gpg-agent.pinentryFlavor = "gtk2";

  services.redshift = {
    enable = true;
    latitude = 36.8;
    longitude = -76.0;
  };

  services.dropbox.enable = true;
  services.flameshot.enable = true;
  home.file.".Xresources_ore".text = ''
    Xft.dpi: 192
  '';
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

      if xrandr | grep -q "HDMI-A-0 connected"; then
        xrandr --output eDP --off --output HDMI-A-0 --primary
      fi

      if lsmod | grep -q "thinkpad"; then
         xrdb -merge ~/.Xresources_ore
        # swap caps and escape on the internal keyboard of ORE
        setxkbmap -device $(xinput list | grep 'AT Translated Set 2 keyboard' | grep -o 'id=[0-9]*' | grep -o '[0-9]*') -option "caps:swapescape"
      fi

      exec ${pkgs.haskellPackages.xmonad}/bin/xmonad
    '';
    executable = true;
  };
  home.file.".xmobarrc".text = import ../../../modules/home/xmobar.nix;
  home.file.".config/hypr/hyprland.conf".text = builtins.readFile ../../../modules/home/hyprland.conf;
  programs.zsh.shellAliases = {
    startx = "exec startx"; # ensures logout after x ends
  };
  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [obs-command-source];
  };
}

{
  pkgs,
  lib,
  ...
}:
let
  theme = import ./home/theme.nix;
in
{
  home.packages = with pkgs; [
    toolbox
    gdb
    linuxKernel.packages.linux_5_15.perf
    bubblewrap
    psmisc
    gnome3.adwaita-icon-theme
    playerctl
    pavucontrol
    pulseaudio-ctl
    xclip
    dmenu
    graphviz
    haskellPackages.xmobar
    haskellPackages.xmonad
    parted
    activate-chrome-tab
    brightnessctl
    gnome.nautilus
    libnotify
    pulseaudioFull
    yubikey-manager-qt
  ];

  home.file.".zshextra".text = ''
    if [[ -f "$HOME/.cargo/env" ]]; then
      source "$HOME/.cargo/env"
    fi
    alias yggit=$HOME/repos/yggit/target/debug/yggit

    #### Zlong alert ####
    # Plays an alert for long-running commands
    DONE_WAV=${./home/done.wav}
    ${builtins.readFile ../overlays/scripts/zlong_alert.zsh}
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
      follow = "mouse";
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
  home.file.".xmobarrc".text = import ./home/xmobar.nix;
  programs.zsh.shellAliases = {
    startx = "exec startx"; # ensures logout after x ends
  };
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/xhtml+xml" = "chromium.desktop";
      "text/html" = "chromium.desktop";
      "text/xml" = "chromium.desktop";
      "x-scheme-handler/ftp" = "chromium.desktop";
      "x-scheme-handler/http" = "chromium.desktop";
      "x-scheme-handler/https" = "chromium.desktop";
    };
  };
  home.sessionVariables = {
    BROWSER = "${lib.getExe pkgs.chromium}";
    TERMINAL = "${lib.getExe pkgs.kitty}";
  };
}

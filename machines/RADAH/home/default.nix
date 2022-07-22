{ pkgs, cfg, ... }:
let
  homeDirectory = "/home/d4hines";
  username = "d4hines";
  theme = import ../../../modules/home/theme.nix;
in
{
  home.stateVersion = "21.11";
  home.packages =
    with pkgs; [
      perf-tools
      watchexec
      bubblewrap
      psmisc
      chkrootkit
      deploy-rs.deploy-rs
      gnome3.adwaita-icon-theme
      playerctl
      pavucontrol
      xclip
      signal-desktop
      dmenu
      graphviz
      google-chrome
      haskellPackages.xmobar
      haskellPackages.xmonad
      picom
      mailspring
      parted
      activate-chrome-tab
      clone-bare-for-worktrees
      mgba
      exercism
      inkscape
      vscode-fhs
      kitty
      obs-studio
      pulseaudio-ctl
      zoom
      zotero

      rnix-lsp
      nixfmt
      haskellPackages.nix-derivation
    ];



  programs.gpg.enable = true;
  services.gpg-agent.enable = true;
  services.gpg-agent.enableScDaemon = true;
  services.gpg-agent.enableSshSupport = true;
  services.gpg-agent.defaultCacheTtl = 60;
  services.gpg-agent.maxCacheTtl = 120;
  services.gpg-agent.sshKeys = [ "0x26D64B46D60FE2BB" ];

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

  services.dropbox.enable = true;
  services.flameshot.enable = true;
  systemd.user.services = with pkgs; {
    twitch-notifications = twitch-notifications-service;
  };
  home.file.".xinitrc" = {
    text = ''
      [ -f ~/.xprofile ] && . ~/.xprofile

      # For GNOME keyring 
      dbus-update-activation-environment --systemd DISPLAY
      eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh) export SSH_AUTH_SOCK

      # Fix weird cursor in some GTK apps
      xsetroot -cursor_name left_ptr

      exec ${pkgs.haskellPackages.xmonad}/bin/xmonad
    '';
    executable = true;
  };
  home.file.".xmobarrc".text = (import ./xmobar.nix) { isNixOS = true; };
  services.vscode-server = {
    enable = true;
    useFhsNodeEnvironment = true;
  };
}

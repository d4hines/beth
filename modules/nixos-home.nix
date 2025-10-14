{
  pkgs,
  ...
}:
let
  theme = import ./home/theme.nix;
in
{
  home.packages = with pkgs; [
    adwaita-icon-theme
    playerctl
    pavucontrol
    pulseaudio-ctl
    xclip
    # dmenu
    rofi
    graphviz
    haskellPackages.xmobar
    haskellPackages.xmonad
    activate-chrome-tab
    brightnessctl
    nautilus
    libnotify
    pulseaudioFull
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
  home.file.".xmobarrc".text = import ./home/xmobar.nix;
}

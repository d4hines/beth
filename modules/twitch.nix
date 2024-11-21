{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.twitch-notifications;
in {
  options.services.twitch-notifications = {
    enable = mkEnableOption "Enable twitch notifications";
  };
  config = mkIf cfg.enable {
    systemd.user.services.twitch-notifications = {
      Unit = {
        Description = "Twitch notification daemon.";
        After = ["network.target"];
      };
      Install = {WantedBy = ["default.target"];};
      Service = {
        ExecStart = "${pkgs.twitch-notification-daemon}/bin/twitch-notification-daemon";
        Restart = "always";
      };
    };
  };
}

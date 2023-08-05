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
      description = "Twitch notification daemon.";

      wantedBy = ["graphical-session.target"];
      after = ["network.target"]; # if networking is needed

      restartIfChanged = true; # set to false, if restarting is problematic

      serviceConfig = {
        ExecStart = "${pkgs.twitch-notification-daemon}/bin/twitch-notification-daemon";
        Restart = "always";
      };
    };
  };
}

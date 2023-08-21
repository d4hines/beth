{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.tagtime;
in {
  options.services.tagtime = {
    enable = mkEnableOption "Enable tagtime daemon";
    graphical = mkEnableOption "If true, sends dunst notification, omitting the rest of the functionality";
  };
  config = mkIf cfg.enable {
    systemd.user.services.tagtime = {
      description = "Tagtime daemon.";
      wantedBy = ["default.target"];
      after = ["network.target"]; # if networking is needed
      restartIfChanged = true; # set to false, if restarting is problematic
      serviceConfig = {
        ExecStart = "${pkgs.tag-time}/bin/tag-time ${
          if cfg.graphical
          then "--graphical"
          else ""
        }";
        Restart = "always";
      };
    };
  };
}

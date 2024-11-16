{ pkgs
, config
, ...
}: {
  age.secrets.ezra-token.file = ../../secrets/ezra-token.age;
  age.secrets.eds-survey-api-token.file = ../../secrets/eds-survey-api-token.age;
  age.secrets.roam-token.file = ../../secrets/roam-token.age;
  age.secrets.rote-server-token.file = ../../secrets/rote-server-token.age;
  users.groups.cloudflared = { };
  users.users.cloudflared = {
    isSystemUser = true;
    hashedPassword = "*";
    group = "cloudflared";
  };
  systemd.services.ssh-tunnel = {
    description = "SSH Tunnel";
    environment = { };
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ]; # if networking is needed
    wants = [ "network-online.target" ];

    restartIfChanged = true; # set to false, if restarting is problematic

    serviceConfig = {
      ExecStart = "${pkgs.cloudflared}/bin/cloudflared tunnel run";
      EnvironmentFile = config.age.secrets.ezra-token.path;
      Restart = "on-failure";
      User = "cloudflared";
      Group = "cloudflared";
      ReadWritePaths = [ ];
      PrivateTmp = "true";
      ProtectSystem = "full";
      NoNewPrivileges = "true";
    };
  };
  systemd.services.eds-survey-api-tunnel = {
    description = "EDS Survey API Tunnel";
    environment = { };
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ]; # if networking is needed
    wants = [ "network-online.target" ];

    restartIfChanged = true; # set to false, if restarting is problematic

    serviceConfig = {
      ExecStart = "${pkgs.cloudflared}/bin/cloudflared tunnel run";
      EnvironmentFile = config.age.secrets.eds-survey-api-token.path;
      Restart = "on-failure";
      User = "cloudflared";
      Group = "cloudflared";
      ReadWritePaths = [ ];
      PrivateTmp = "true";
      ProtectSystem = "full";
      NoNewPrivileges = "true";
    };
  };
  systemd.services.eds-survey-api = {
    description = "EDS Survey API";

    environment = {
      PATH_TO_SURVEY = "/home/d4hines/OneDrive/eds_data.xlsx";
    };
    path = [ pkgs.xlsx2csv ];
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ]; # if networking is needed
    wants = [ "network-online.target" ];

    restartIfChanged = true; # set to false, if restarting is problematic
    serviceConfig = {
      ExecStart = "${pkgs.bun}/bin/bun /home/d4hines/eds_survey/index.ts";
      Restart = "on-failure";
      User = "d4hines";
      ReadWritePaths = [ ];
      PrivateTmp = "true";
      ProtectSystem = "full";
      NoNewPrivileges = "true";
    };
  };
  systemd.services.roam-recurring-tasks = {
    description = "Roam Recurring Tasks";
    environment = { };
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];

    restartIfChanged = true; # set to false, if restarting is problematic
    serviceConfig = {
      ExecStart = "${pkgs.roam-recurring-tasks}/bin/roam-recurring-tasks";
      EnvironmentFile = config.age.secrets.roam-token.path;
      Restart = "on-failure";
      User = "d4hines";
    };
  };
  systemd.services.rote-server-tunnel = {
    description = "Rote Server Tunnel";
    environment = { };
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ]; # if networking is needed
    wants = [ "network-online.target" ];
    restartIfChanged = true; # set to false, if restarting is problematic
    serviceConfig = {
      ExecStart = "${pkgs.cloudflared}/bin/cloudflared tunnel run";
      EnvironmentFile = config.age.secrets.rote-server-token.path;
      Restart = "on-failure";
      User = "cloudflared";
      Group = "cloudflared";
      ReadWritePaths = [ ];
      PrivateTmp = "true";
      ProtectSystem = "full";
      NoNewPrivileges = "true";
    };
  };
}

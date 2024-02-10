{
  pkgs,
  config,
  ...
}: {
  age.secrets.ezra-token.file = ../../secrets/ezra-token.age;
  age.secrets.eds-survey-api-token.file = ../../secrets/eds-survey-api-token.age;
  age.secrets.roam-token.file = ../../secrets/roam-token.age;
  users.groups.roam = {};
  users.users.roam = {
    isSystemUser = true;
    hashedPassword = "*";
    group = "roam";
  }; 
  users.groups.cloudflared = {};
  users.users.cloudflared = {
    isSystemUser = true;
    hashedPassword = "*";
    group = "cloudflared";
  };
  users.groups.edssurvey = {};
  users.users.edssurvey = {
    isSystemUser = true;
    hashedPassword = "*";
    group = "edssurvey";
  };
  systemd.services.ssh-tunnel = {
    description = "SSH Tunnel";
    environment = {};
    wantedBy = ["multi-user.target"];
    after = ["network.target"]; # if networking is needed

    restartIfChanged = true; # set to false, if restarting is problematic

    serviceConfig = {
      ExecStart = "${pkgs.cloudflared}/bin/cloudflared tunnel run";
      EnvironmentFile = config.age.secrets.ezra-token.path;
      Restart = "on-failure";
      User = "cloudflared";
      Group = "cloudflared";
      ReadWritePaths = [];
      PrivateTmp = "true";
      ProtectSystem = "full";
      NoNewPrivileges = "true";
    };
  };
  systemd.services.eds-survey-api-tunnel = {
    description = "EDS Survey API Tunnel";
    environment = {};
    wantedBy = ["multi-user.target"];
    after = ["network.target"]; # if networking is needed

    restartIfChanged = true; # set to false, if restarting is problematic

    serviceConfig = {
      ExecStart = "${pkgs.cloudflared}/bin/cloudflared tunnel run";
      EnvironmentFile = config.age.secrets.eds-survey-api-token.path;
      Restart = "on-failure";
      User = "cloudflared";
      Group = "cloudflared";
      ReadWritePaths = [];
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
    wantedBy = ["multi-user.target"];
    after = ["network.target"]; # if networking is needed

    restartIfChanged = true; # set to false, if restarting is problematic
    serviceConfig = {
      ExecStart = "${pkgs.bun}/bin/bun /home/d4hines/eds_survey/index.ts";
      Restart = "on-failure";
      User = "edssurvey";
      Group = "edssurvey";
      ReadWritePaths = [];
      PrivateTmp = "true";
      ProtectSystem = "full";
      NoNewPrivileges = "true";
    };
  };
 systemd.services.roam-recurring-tasks = {
    description = "Roam Recurring Tasks";
    environment = {};
    wantedBy = ["multi-user.target"];
    after = ["network.target"]; # if networking is needed

    restartIfChanged = true; # set to false, if restarting is problematic

    serviceConfig = {
      ExecStart = "${pkgs.roam-recurring-tasks}/bin/roam-recurring-tasks";
      EnvironmentFile = config.age.secrets.roam-token.path;
      Restart = "on-failure";
      User = "roam";
      Group = "roam";
      ReadWritePaths = [];
      PrivateTmp = "true";
      ProtectSystem = "full";
      NoNewPrivileges = "true";
    };
  } ;
  virtualisation.oci-containers = {
    containers.homeassistant = {
      volumes = ["/home_assistant_config:/config"];
      environment.TZ = "America/New_York";
      image = "ghcr.io/home-assistant/home-assistant:stable"; # Warning: if the tag does not change, the image will not be updated
      extraOptions = [
        "--network=host"
      ];
    };
  };
  services.mosquitto = {
    enable = true;
    listeners = [
      {
        users.nlpc = {
          acl = [
            "readwrite #"
          ];
          hashedPassword = "$7$101$oIuxk3CfOqBD4fj3$aFhh37r7QFW50Mx80k5m3UkGKMfkOhMeivc9gMOj6nTT5NtExt/dRrYtQKm1wtmYlpiZdIYWzogXRHUHkiIvrg==";
        };
      }
    ];
  };
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [1883];
  };
}

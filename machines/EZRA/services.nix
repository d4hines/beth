{
  pkgs,
  config,
  ...
}: {
  age.secrets.ezra-token.file = ../../secrets/ezra-token.age;
  users.groups.cloudflared = {};
  users.users.cloudflared = {
    isSystemUser = true;
    hashedPassword = "*";
    group = "cloudflared";
  };
  systemd.services.ssh-tunnel = {
    description = "SSH Tunnel";
    environment = {
    };
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
}

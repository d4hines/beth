{ complice-xmobar }:
let makeService = { name, description, serviceConfig, environment ? { } }:
  { ... }: {
    users.users."${name}" = {
      isSystemUser = true;
      hashedPassword = "*";
      group = "${name}";
    };
    users.groups."${name}" = {};
    systemd.services."${name}" = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      inherit description environment;
      serviceConfig = serviceConfig // { User = name; };
    };
  };
in
[
  (makeService {
    name = "complice-xmobar";
    description = "Complice-xmobar integration server";
    environment = {
      "COMPLICE_TOKEN" = builtins.readFile ../../secrets/complice_api;
    };
    serviceConfig = {
      ExecStart =
        "${complice-xmobar.packages.aarch64-linux.complice-xmobar}/lib/node_modules/complice-xmobar/complice.js";
    };
  })
]


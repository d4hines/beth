{ config, pkgs, ... }: {
  virtualisation.oci-containers.containers = {
    complice-xmobar = {
      image = "node";
      environment = {
        "COMPLICE_TOKEN" = builtins.readFile ../secrets/complice_api;
      };
      volumes = [ "${pkgs.complice-xmobar}/app:/app" ];
      entrypoint = "/app/complice.js";
      ports = [ "7000:7000" ];
    };
  };
}

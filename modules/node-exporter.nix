{...}: {
  # node-exporter
  # Gets us metrics about the nodes in prometheus
  services.prometheus = {
    exporters = {
      node = {
        enable = true;
        enabledCollectors = ["systemd"];
        port = 9002;
      };
    };
  };

  # node-exorter port
  networking.firewall.allowedTCPPorts = [9002];
}

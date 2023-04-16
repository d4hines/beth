{loki_ip}: {config, ...}: {
  # Exports our logs to loki
  services.promtail = {
    enable = true;
    configuration = {
      server = {
        http_listen_port = 28183;
        grpc_listen_port = 0;
      };

      positions.filename = "/tmp/positions.yaml";

      clients = [
        {
          url = "http://${loki_ip}:3100/loki/api/v1/push";
        }
      ];

      limits_config = {
        readline_rate_enabled = true;
        readline_rate = 50;
        readline_burst = 50;
      };

      scrape_configs = [
        {
          job_name = "journal";
          journal = {
            max_age = "12h";
            labels = {
              job = "systemd-journal";
              host = config.networking.hostName;
            };
          };

          relabel_configs = [
            {
              source_labels = ["__journal__systemd_unit"];
              target_label = "unit";
            }
          ];
        }
      ];
    };
  };
}

{
  hardware-module,
  rev,
}: {
  system = "aarch64-linux";
  modules =
    (import ./services.nix)
    ++ [
      hardware-module
      ../../modules/avahi.nix
      ({pkgs, ...}: {
        fileSystems = {
          "/" = {
            device = "/dev/disk/by-label/NIXOS_SD";
            fsType = "ext4";
            options = ["noatime"];
          };
        };
        nix = {
          extraOptions = ''
            require-sigs = false
            experimental-features = nix-command flakes
          '';
        };
        networking.hostName = "ARCTURUS";
        time.timeZone = "America/New_York";
        environment.systemPackages = with pkgs; [
          vim
          htop
          git
        ];
        environment.etc."revision".text = "${rev}";

        services.openssh = {
          enable = true;
          settings.PasswordAuthentication = false;
        };
        networking.firewall.allowedTCPPorts = [80 443];

        security.acme.acceptTerms = true;
        security.acme.defaults.email = "d4hines@gmail.com";
        services.nginx = {
          enable = true;
          virtualHosts."hines.house" = {
            enableACME = true;
            forceSSL = true;
            root = "/var/www/home";
          };
          virtualHosts."sub.hines.house" = {
            enableACME = true;
            forceSSL = true;
            root = "/var/www/sub";
          };
          virtualHosts."commit.hines.house" = {
            enableACME = true;
            forceSSL = true;
            root = "/var/www/commit/build";
          };
          virtualHosts."fox-clan.hines.house" = {
            enableACME = true;
            forceSSL = true;
            extraConfig = ''
              server_name fox-clan.hines.house;
              return 301 https://meet.google.com/ood-udne-kdr;
            '';
          };
          virtualHosts."inventory.hines.house" = {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              proxyPass = "http://192.168.0.139:9090";
            };
          };
          # how to do a TLS reverse proxy for running a service behind HTTPS
          # https://nixos.wiki/wiki/Nginx#TLS_reverse_proxy
          # Extra steps
          # - Add the A record in your DNS
          # - Make sure router is pointing to right computer on right port
          # - Open port in computer's firewall
          # e.g.
          # virtualHosts."sub1.hines.house" = {
          #   enableACME = true;
          #   forceSSL = true;
          #   locations."/" = {
          #     proxyPass = "http://192.168.0.X:8080";
          #   };
          # };
        };

        users = {
          mutableUsers = false;
          users."d4hines" = {
            isNormalUser = true;
            hashedPassword = builtins.readFile ../../secrets/password;
            extraGroups = ["wheel"];
            openssh.authorizedKeys.keyFiles = [../../keys/authorized_keys];
          };
        };
        programs.zsh.enable = true;
        programs.tmux.enable = true;
        security.sudo.wheelNeedsPassword = false;

        # This value determines the NixOS release from which the default
        # settings for stateful data, like file locations and database versions
        # on your system were taken. It‘s perfectly fine and recommended to leave
        # this value at the release version of the first install of this system.
        # Before changing this value read the documentation for this option
        # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
        system.stateVersion = "21.11"; # Did you read the comment?
      })
    ];
}

{
  all-overlays,
  home,
  fix-nixpkgs-path,
  tezos,
  rev,
}: {
  system = "x86_64-linux";
  modules = [
    ({...}: {nixpkgs.overlays = all-overlays;})
    ../../modules/sound.nix
    ./cron.nix
    ./hardware-configuration.nix
    ../../modules/avahi.nix
    ../../modules/services.nix
    fix-nixpkgs-path
    home.nixosModules.home-manager
    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.d4hines = {...}: {
        imports = [
          (import ./home {inherit tezos;})
          ../../modules/home
        ];
      };
    }
    ({pkgs, ...}: {
      # Use the systemd-boot EFI boot loader.
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.grub.extraEntries = ''
        menuentry "Arch" {
          search --set=arch --fs-uuid f31314b3-8177-4ef2-8623-d4445c46c885
          configfile "($arch)/boot/grub/grub.cfg"
        }
      '';
      # Enable cross-compiling
      boot.binfmt.emulatedSystems = ["aarch64-linux"];
      nix = {
        package = pkgs.nixFlakes;

        extraOptions = ''
          experimental-features = nix-command flakes
          extra-platforms = aarch64-linux

          extra-substituters = https://ocaml.nix-cache.com
          extra-trusted-public-keys = ocaml.nix-cache.com-1:/xI2h2+56rwFfKyyFVbkJSeGqSIYMC/Je+7XXqGKDIY=

          extra-substituters = https://tezos.nix-cache.workers.dev
          extra-trusted-public-keys = tezos-nix-cache.marigold.dev-1:4nS7FPPQPKJIaNQcbwzN6m7kylv16UCWWgjeZZr2wXA=
        '';
        # trusted-users = [ "@wheel" ];
        # trusted-substituters = [
        #   "https://nix-community.cachix.org"
        #   "https://anmonteiro.cachix.org"
        # ];
        # trusted-public-keys = [
        #   "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        #   "anmonteiro.cachix.org-1:KF3QRoMrdmPVIol+I2FGDcv7M7yUajp4F2lt0567VA4="
        # ];
      };
      nixpkgs.config.allowUnfree = true;
      nixpkgs.config.permittedInsecurePackages = [
        "nodejs-16.20.0"
      ];
      networking.hostName = "RADAH"; # Define your hostname.
      networking.extraHosts = ''
        127.0.0.1 twitter.com
      '';
      networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.
      networking.wireless.networks."${builtins.readFile ../../secrets/ssid}".psk = builtins.readFile ../../secrets/wifi_psk;
      networking.wireless.userControlled.enable = true;

      time.timeZone = "America/New_York";

      # The global useDHCP flag is deprecated, therefore explicitly set to false here.
      # Per-interface useDHCP will be mandatory in the future, so this generated config
      # replicates the default behaviour.
      networking.useDHCP = false;
      networking.interfaces.enp39s0.useDHCP = true;
      networking.interfaces.wlan0.useDHCP = true;

      i18n.defaultLocale = "en_US.UTF-8";

      services.devmon.enable = true;

      services.xserver.enable = true;
      services.xserver.dpi = 96;
      services.xserver.displayManager.startx.enable = true;

      services.xserver.layout = "us";

      services.gnome.gnome-keyring.enable = true;
      security.pam.services.sddm.enableGnomeKeyring = true;
      programs.seahorse.enable = true;

      users.defaultUserShell = pkgs.zsh;
      programs.zsh.enable = true;
      users.users.d4hines = {
        isNormalUser = true;
        hashedPassword = builtins.readFile ../../secrets/password;
        extraGroups = ["wheel" "networkmanager" "docker"];
        openssh.authorizedKeys.keyFiles = [../../keys/authorized_keys];
      };
      security.sudo.wheelNeedsPassword = false;

      services.getty.autologinUser = "d4hines";

      services.pcscd.enable = true;

      virtualisation.docker.enable = true;
      # enable VirtualBox
      # virtualisation.virtualbox.host.enable = true;
      # virtualisation.virtualbox.host.enableExtensionPack = true;
      # users.extraGroups.vboxusers.members = ["d4hines"];

      programs.command-not-found.enable = true;

      # List packages installed in system profile. To search, run:
      # $ nix search wget
      environment.systemPackages = with pkgs; [
        git
        wget
        efibootmgr

        # To fix missing icons for GTK apps like pavucontrol
        gnome3.adwaita-icon-theme

        signal-desktop
      ];

      environment.etc."revision".text = "${rev}";
      # Also required to fix missing icons in GTK apps
      services.dbus.packages = with pkgs; [dconf];

      # grafana configuration
      services.grafana = {
        enable = true;
        settings = {
          server.http_port = 2342;
          server.http_addr = "127.0.0.1";
          auth = {
            disable_login_form = true;
            login_cookie_name = "_oauth2_proxy";
            oauth_auto_login = true;
            # signout_redirect_url = "https://grafana.${hostName}.meurer.org/oauth2/sign_out?rd=https%3A%2F%2Fgrafana.${hostName}.meurer.org";
          };
          "auth.basic".enabled = false;
          "auth.proxy" = {
            enabled = true;
            auto_sign_up = true;
            enable_login_token = false;
            header_name = "X-Email";
            header_property = "email";
          };
          users = {
            allow_signup = false;
            auto_assign_org = true;
            auto_assign_org_role = "Viewer";
          };
        };

        provision.datasources.settings.datasources = [
          {
            type = "prometheus";
            name = "prometheus";
            url = "http://localhost:9002";
          }
        ];
      };

      services.twitch-notifications.enable = true;

      services.prometheus = {
        enable = true;
        exporters = {
          node = {
            enable = true;
            enabledCollectors = ["systemd"];
            port = 9002;
          };
        };
        scrapeConfigs = [
          {
            job_name = "foo";
            static_configs = [
              {
                targets = ["127.0.0.1:9002"];
              }
            ];
          }
        ];
      };

      # Enable the OpenSSH daemon.
      services.openssh = {
        enable = true;
        settings.PasswordAuthentication = false;
        ports = [7846];
      };
      networking.nameservers = ["1.1.1.1" "9.9.9.9"];

      networking.firewall.allowedTCPPorts = [19000 9090];

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

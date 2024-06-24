{
  all-overlays,
  home,
  fix-nixpkgs-path,
  rev,
}: {
  system = "x86_64-linux";
  modules = [
    ({...}: {nixpkgs.overlays = all-overlays;})
    ./hardware-configuration.nix
    ../../modules/avahi.nix
    ../../modules/twitch.nix
    fix-nixpkgs-path
    home.nixosModules.home-manager
    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.d4hines = {...}: {
        imports = [
          (import ./home)
          ../../modules/home
        ];
      };
    }
    ({pkgs, ...}: {
      # Use the systemd-boot EFI boot loader.
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.initrd.systemd.enable = true;

      environment.sessionVariables.NIXOS_OZONE_WL = "1";

      services.pipewire.enable = true;
      services.pipewire.audio.enable = true;
      services.pipewire.wireplumber.enable = true;

      # Enable cross-compiling
      boot.binfmt.emulatedSystems = ["aarch64-linux"];
      nix = {
        package = pkgs.nixFlakes;

        extraOptions = ''
          experimental-features = nix-command flakes
          extra-platforms = aarch64-linux
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
      networking.hostName = "YASAB"; # Define your hostname.
      networking.networkmanager.enable = true;
      networking.networkmanager.wifi.powersave = false;
      # networking.extraHosts = ''
      #   127.0.0.1 twitter.com
      # '';
      #networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.
      # networking.wireless.networks."${builtins.readFile ../../secrets/ssid}".psk = builtins.readFile ../../secrets/wifi_psk;
      #networking.wireless.userControlled.enable = true;

      time.timeZone = "America/New_York";

      # The global useDHCP flag is deprecated, therefore explicitly set to false here.
      # Per-interface useDHCP will be mandatory in the future, so this generated config
      # replicates the default behaviour.
      networking.useDHCP = false;

      i18n.defaultLocale = "en_US.UTF-8";

      services.devmon.enable = true;

      services.xserver.enable = true;
      services.xserver.dpi = 96;
      services.xserver.displayManager.startx.enable = true;

      services.xserver.xkb.layout = "us";

      services.gnome.gnome-keyring.enable = true;
      security.pam.services.sddm.enableGnomeKeyring = true;
      programs.seahorse.enable = true;
      programs.zsh.enable = true;
      users.users.d4hines = {
        shell = pkgs.zsh;
        isNormalUser = true;
        extraGroups = ["wheel" "networkmanager" "docker" "video"];
        openssh.authorizedKeys.keyFiles = [../../keys/authorized_keys];
      };
      security.sudo.wheelNeedsPassword = false;
      services.openssh = {
        enable = true;
        settings.PasswordAuthentication = false;
      };
      # security.pam.yubico.enable = true;
      programs.yubikey-touch-detector.enable = true;

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
        vim
        wofi
        waybar
        waypipe
        wayvnc

        # To fix missing icons for GTK apps like pavucontrol
        gnome3.adwaita-icon-theme
        networkmanagerapplet
        signal-desktop
        yubikey-touch-detector
        pinentry-gtk2
      ];
      programs.nix-ld.enable = true;
      programs.nix-ld.libraries = with pkgs; [
        # Add any missing dynamic libraries for unpackaged programs
        # here, NOT in environment.systemPackages
      ];

      environment.etc."revision".text = "${rev}";
      # Also required to fix missing icons in GTK apps
      services.dbus.packages = with pkgs; [dconf];

      programs.hyprland.enable = true;
      programs.waybar.enable = true;
      #services.twitch-notifications.enable = true;

      programs.i3lock = {
        enable = true;
        u2fSupport = true;
      };
      systemd.services.i3lock = {
        description = "i3lock";
        environment = {
          DISPLAY = ":0";
        };
        serviceConfig = {
          User = "d4hines";
          Type = "forking";
          ExecStart = "${pkgs.i3lock}/bin/i3lock -i /home/d4hines/lock-screen.png";
        };
      };
      services.udev.extraRules = ''ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0407", RUN+="${pkgs.systemd}/bin/systemctl start --no-block i3lock.service"'';

      networking.nameservers = ["1.1.1.1" "9.9.9.9"];
      networking.firewall.enable = false;

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

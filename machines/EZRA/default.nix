# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  rev,
  all-overlays,
  fix-nixpkgs-path,
  agenix,
}: {
  system = "x86_64-linux";
  modules = [
    agenix.nixosModules.default
    ./services.nix
    ({...}: {nixpkgs.overlays = all-overlays;})
    ./hardware-configuration.nix
    ../../modules/avahi.nix
    ../../modules/node-exporter.nix
    ({pkgs, ...}: {
      # Bootloader.
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.efi.efiSysMountPoint = "/boot/efi";

      # Enable cross-compiling
      boot.binfmt.emulatedSystems = ["aarch64-linux"];

      networking.hostName = "EZRA"; # Define your hostname.
      networking.nameservers = ["1.1.1.1" "9.9.9.9"];
      #networking.firewall.allowedTCPPorts = [];

      services.openssh = {
        enable = true;
        settings.PasswordAuthentication = false;
      };
      # Set your time zone.
      time.timeZone = "America/New_York";

      # Select internationalisation properties.
      i18n.defaultLocale = "en_US.UTF-8";

      # Define a user account. Don't forget to set a password with ‘passwd’.
      users.users.d4hines = {
        isNormalUser = true;
        description = "Daniel Hines";
        extraGroups = ["networkmanager" "wheel"];
        openssh.authorizedKeys.keyFiles = [../../keys/authorized_keys];
      };
      security.sudo.wheelNeedsPassword = false;

      users.users.rote = {
        isNormalUser = true;
        description = "Rote User";
        group = "rote";
      };
      users.groups.rote = {};

      nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
          require-sigs = false
          experimental-features = nix-command flakes
          extra-platforms = aarch64-linux
        '';
      };

      # Allow unfree packages
      nixpkgs.config.allowUnfree = true;

      # List packages installed in system profile. To search, run:
      # $ nix search wget
      environment.systemPackages = with pkgs; [
        hello
        yarn
        vim
        git
        neofetch
        htop
        toolbox
        xlsx2csv # For EDS survey
      ];

      environment.etc."revision".text = "${rev}";
      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = "22.11"; # Did you read the comment?
    })
  ];
}

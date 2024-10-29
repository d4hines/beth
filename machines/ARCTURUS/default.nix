{
  hardware-module,
  home,
  rev,
  all-overlays,
  fix-nixpkgs-path,
}: {
  system = "aarch64-linux";
  modules =
    [
      ({...}: {nixpkgs.overlays = all-overlays;})
      hardware-module
      # ../../modules/sound.nix
      ../../modules/avahi.nix
      # fix-nixpkgs-path
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
        nixpkgs.config.allowUnfree = true;
        networking.hostName = "ARCTURUS";
        networking.networkmanager.enable = true;
        networking.networkmanager.wifi.powersave = false;
        # The global useDHCP flag is deprecated, therefore explicitly set to false here.
        # Per-interface useDHCP will be mandatory in the future, so this generated config
        # replicates the default behaviour.
        networking.useDHCP = false;

        networking.nameservers = ["1.1.1.1" "9.9.9.9"];
        time.timeZone = "America/New_York";
        environment.systemPackages = with pkgs; [
          vim
          htop
          pavucontrol
          # mednafen
          # mednaffe
          # superTuxKart
          # chromium
          haskellPackages.play-xmonad
        ];
        programs.nix-ld.enable = true;
        programs.nix-ld.libraries = with pkgs; [
          # Add any missing dynamic libraries for unpackaged programs
          # here, NOT in environment.systemPackages
        ];

        environment.etc."revision".text = "${rev}";

        services.openssh = {
          enable = true;
          settings.PasswordAuthentication = false;
        };

        services.xserver.enable = true;
        services.xserver.dpi = 96;
        services.xserver.displayManager.startx.enable = true;

        users = {
          mutableUsers = false;
          users."d4hines" = {
            isNormalUser = true;
            extraGroups = ["wheel" "networkmanager" "video"];
            openssh.authorizedKeys.keyFiles = [../../keys/authorized_keys];
          };
        };
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

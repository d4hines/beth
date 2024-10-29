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
        networking.hostName = "ARCTURUS";
        time.timeZone = "America/New_York";
        environment.systemPackages = with pkgs; [
          vim
          htop
          git
          toolbox

          nodejs
          yarn
        ];
        environment.etc."revision".text = "${rev}";

        services.openssh = {
          enable = true;
          settings.PasswordAuthentication = false;
        };

        users = {
          mutableUsers = false;
          users."d4hines" = {
            isNormalUser = true;
            extraGroups = ["wheel"];
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

{
  home,
  rev,
  disko,
  nixosModules,
  all-overlays,
  ...
}:
{
  system = "x86_64-linux";
  modules = [
    (
      { ... }:
      {
        nixpkgs.overlays = all-overlays;
      }
    )
    disko.nixosModules.disko
    ./disk-config.nix
    ./hardware-configuration.nix
    ./tools.nix
    home.nixosModules.home-manager
    nixosModules.avahi
    nixosModules.sound
    nixosModules.graphical
    nixosModules.firefox
    (
      { pkgs, ... }:
      {
        nixpkgs = {
          config.allowUnfree = true;
        };
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        boot.loader.grub = {
          # no need to set devices, disko will add all devices that have a EF02 partition to the list already
          # devices = [ ];
          efiSupport = true;
          efiInstallAsRemovable = true;
        };
        networking.hostName = "CHARLIEPC";
        networking.networkmanager.enable = true;
        networking.nameservers = [
          "1.1.1.1"
          "9.9.9.9"
        ];
        time.timeZone = "America/Los_Angeles";
        users.users.charlie = {
          isNormalUser = true;
          description = "Charlie Hines";
          extraGroups = [
            "networkmanager"
            "wheel"
            "transmission"
          ];
          openssh.authorizedKeys.keyFiles = [ ../../keys/authorized_keys ];
        };
        security.sudo.wheelNeedsPassword = false;
        services.xserver.displayManager.autoLogin.user = "charlie";
        services.openssh.enable = true;
        services.openssh.settings.X11Forwarding = true;
        users.users.root.openssh.authorizedKeys.keyFiles = [ ../../keys/authorized_keys ];
        nix = {
          extraOptions = ''
            experimental-features = nix-command flakes
          '';
        };
        services.xserver.videoDrivers = [ "amdgpu" ];

        hardware.opengl.enable = true;
        fonts.fontconfig.enable = true;
        hardware.bluetooth.enable = true;
        hardware.bluetooth.powerOnBoot = true;
        # Let non-root control bluetooth (bluetoothctl uses polkit)
        security.polkit.enable = true;
        environment.systemPackages = with pkgs; [
          bluez
          x11vnc
          hmcl
        ];
        programs.nix-ld.enable = true;
        programs.nix-ld.libraries = [
          # Add any missing dynamic libraries for unpackaged programs
          # here, NOT in environment.systemPackages
        ];
        environment.etc."revision".text = "${rev}";

        system.stateVersion = "23.11";
      }
    )
    (
      { ... }:
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.charlie =
          { ... }:
          {
            imports = [
              (
                { ... }:
                {
                  home.stateVersion = "24.05";
                }
              )
              (nixosModules.home {
                gitUserName = "Daniel Hines";
                gitUserEmail = "d4hines@gmail.com";
              })
              nixosModules.nixos-home
            ];
          };
      }
    )
  ];
}

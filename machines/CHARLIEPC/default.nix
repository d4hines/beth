{
  rev,
  disko,
  nixosModules,
  ...
}:
{
  system = "x86_64-linux";
  modules = [
    disko.nixosModules.disko
    ./disk-config.nix
    ./hardware-configuration.nix
    ./tools.nix
    nixosModules.avahi
    nixosModules.sound
    nixosModules.graphical
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
          ];
          openssh.authorizedKeys.keyFiles = [ ../../keys/authorized_keys ];
        };
        security.sudo.wheelNeedsPassword = false;
        services.xserver.displayManager.autoLogin.user = "charlie";
        services.openssh.enable = true;
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
          firefox
          bluez
          x11vnc
          hmcl
        ];
        environment.etc."revision".text = "${rev}";
        system.stateVersion = "23.11";
      }
    )
  ];
}

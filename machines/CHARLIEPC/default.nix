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
        services.getty.autologinUser = "charlie";
        services.openssh.enable = true;
        users.users.root.openssh.authorizedKeys.keyFiles = [ ../../keys/authorized_keys ];
        nix = {
          extraOptions = ''
            experimental-features = nix-command flakes
          '';
        };
        services.xserver = {
          enable = true;
          layout = "us";
          xkbVariant = "";
          displayManager.startx.enable = true;
          videoDrivers = [ "amdgpu" ];
        };
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
        services.xserver.displayManager.sessionCommands = ''
          # Share the running :0 session, no VNC password, localhost only
          ${pkgs.x11vnc}/bin/x11vnc \
            -display :0 \
            -localhost \
            -nopw \
            -forever -shared \
            -rfbport 5900 \
            -o "$HOME/.x11vnc.log" &
        '';
        environment.etc."revision".text = "${rev}";
        system.stateVersion = "23.11";
      }
    )
  ];
}

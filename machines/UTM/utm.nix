{ username }:
{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "sr_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # Enables scripting: https://docs.getutm.app/scripting/scripting/
  services.qemuGuest.enable = true;

  # See configuration.nix,
  # https://github.com/utmapp/UTM/issues/4644#issuecomment-2900887629
  services.chrony = {
    enable = true;
    extraConfig = ''
      refclock RTC /dev/rtc0:utc
      makestep 1 -1
    '';
    # We're syncing _from_ the RTC, not writing to it.
    enableRTCTrimming = false;
  };

  fileSystems."/mnt/mac" = {
    device = "share";
    fsType = "virtiofs";
  };
  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp0s1.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
}

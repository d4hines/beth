{ pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.extraEntries = ''
    menuentry "Arch" {
      search --set=arch --fs-uuid f31314b3-8177-4ef2-8623-d4445c46c885
      configfile "($arch)/boot/grub/grub.cfg"
    }
  '';
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://anmonteiro.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "anmonteiro.cachix.org-1:KF3QRoMrdmPVIol+I2FGDcv7M7yUajp4F2lt0567VA4="
    ];
  };

  networking.hostName = "RADAH"; # Define your hostname.
  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp39s0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  i18n.defaultLocale = "en_US.UTF-8";

  services.xserver.enable = true;
  services.xserver.dpi = 96;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.extraPackages = haskellPackages: [
    haskellPackages.xmonad-contrib_0_17_0
  ];
  services.xserver.windowManager.xmonad.config = builtins.readFile ./xmonad.hs;
  services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "d4hines";

  services.xserver.layout = "us";

  services.gnome.gnome-keyring.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.avahi.enable = true;
  services.avahi.nssmdns = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.defaultUserShell = pkgs.zsh;
  users.users.d4hines = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
  };
  security.sudo.wheelNeedsPassword = false;

  services.getty.autologinUser = "d4hines";

  virtualisation.docker.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    wget
    xorg.xdpyinfo
    efibootmgr
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  networking.nameservers = [ "1.1.1.1" "9.9.9.9" ];
  networking.firewall.allowedTCPPorts = [ 7000 ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

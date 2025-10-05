{
  pkgs,
  ...
}:

{
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # On a VM you can usually afford to set the timeout to something
  # shorter.
  boot.loader.timeout = 2;

  # Enable rosetta x86 virtulisation. Comment out if you're using QEMU backend.
  virtualisation.rosetta.enable = true;
  boot.binfmt.emulatedSystems = [ "x86_64-linux" ];

  networking.hostName = "nixos"; # Define your hostname.
  networking.interfaces.enp0s1.useDHCP = true;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  users.users.d4hines = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    psmisc
    wget
    vim
    htop
    nixpkgs-fmt
    cachix
  ];
  # This is really helpful for using VS Code over SSH along side the
  # The direnv extension: https://marketplace.visualstudio.com/items?itemName=mkhl.direnv
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  # This is also required for using VS Code over SSH. See https://nixos.wiki/wiki/Visual_Studio_Code#Remote_SSH
  programs.nix-ld.enable = true;

  programs.nix-ld.libraries = with pkgs; [ ];
  # Add any missing dynamic libraries for unpackaged programs
  # here, NOT in environment.systemPackages

  # Enable your shell of choice.
  programs.zsh.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  nixpkgs = {
    config.allowUnfree = true;
  };

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes fetch-closure
      allow-import-from-derivation = true
      builders-use-substitutes = true
    '';
    settings = {
      trusted-users = [ "@wheel" ];
      # Enable cross-compilation from aarch64 to x86
      extra-platforms = [
        "x86_64-linux"
        "i686-linux"
      ];
    };
  };

  programs.git = {
    enable = true;
    lfs.enable = true;
  };

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11"; # Did you read the comment?
}

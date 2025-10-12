{
  all-overlays,
  home,
  disko,
  rev,
  nixosModules,
}:
{
  system = "aarch64-linux";
  modules = [
    (
      { ... }:
      {
        nixpkgs.overlays = all-overlays;
      }
    )
    nixosModules.utm
    disko.nixosModules.disko
    home.nixosModules.home-manager
    ./disko-config.nix
    (
      { pkgs, ... }:
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
          initialHashedPassword = "$y$j9T$Qas.adn.KE.tM.zKdbw18/$XG.ZcLm8RgcoaKpS15QF9mHoTXNhGivTjO5FneIJ1o5";
          extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
          useDefaultShell = true;
          shell = pkgs.zsh;
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

        # enable file sharing with mac.
        # Note you *must* set this up in UTM, or you'll get
        # an error at boot.
        fileSystems."/mnt/mac" = {
          device = "share";
          fsType = "virtiofs";
        };

        programs.nix-ld.enable = true;
        programs.nix-ld.libraries = [
          # Add any missing dynamic libraries for unpackaged programs
          # here, NOT in environment.systemPackages
        ];

        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.d4hines =
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
            ];
          };
      }
    )
  ];
}

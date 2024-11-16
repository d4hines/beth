{ hardware-module
, home
, rev
, all-overlays
, fix-nixpkgs-path
,
}: {
  system = "aarch64-linux";
  modules = [
    hardware-module
    ./hardware-configuration.nix
    ({ ... }: { nixpkgs.overlays = all-overlays; })
    # ../../modules/sound.nix
    ../../modules/avahi.nix
    fix-nixpkgs-path
    ({ pkgs, ... }: {
      nix = {
        extraOptions = ''
          require-sigs = false
          experimental-features = nix-command flakes
        '';
      };

      # enable audio
      sound.enable = true;
      hardware.pulseaudio.enable = true;
      boot.kernelParams = [ "snd_bcm2835.enable_hdmi=1" "snd_bcm2835.enable_headphones=1" ];
      boot.loader.raspberryPi.firmwareConfig = ''
        dtparam=audio=on
        hdmi_drive=2
      '';

      # Pipewire for sound
      # security.rtkit.enable = true; # Used to acquire realtime priority if needed
      # services.pipewire = {
      #   enable = true;
      #   alsa.enable = true;
      #   alsa.support32Bit = true;
      #   pulse.enable = true;
      #   jack.enable = true;
      #   socketActivation = false; # too slow for headless; start at boot instead
      # };
      # Start WirePlumber (with PipeWire) at boot.
      # systemd.user.services.wireplumber.wantedBy = ["default.target"];

      hardware = {
        raspberry-pi."4" = {
          apply-overlays-dtmerge.enable = true;
          fkms-3d.enable = true;
        };
        bluetooth.enable = false;
        # deviceTree = {
        #   enable = true;
        #   filter = "*rpi-4-*.dtb";
        # };
      };

      networking.hostName = "ARCTURUS";
      networking.networkmanager.enable = true;
      # The global useDHCP flag is deprecated, therefore explicitly set to false here.
      # Per-interface useDHCP will be mandatory in the future, so this generated config
      # replicates the default behaviour.
      networking.useDHCP = false;

      networking.nameservers = [ "1.1.1.1" "9.9.9.9" ];
      time.timeZone = "America/New_York";
      environment.systemPackages = with pkgs; [
        vim
        htop
        tmux
        pavucontrol
        mednafen
        mednaffe
        superTuxKart
        kitty
        dmenu
        chromium
        tmux
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
      services.xserver = {
        enable = true;
        dpi = 96;
      };
      services.xserver.displayManager.startx.enable = true;
      services.xserver.windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = hp: [ hp.xmonad hp.xmonad-contrib hp.xmonad-extras ];
        config = builtins.readFile ./xmonad.hs;
      };

      # services.displayManager = {
      #   lightdm.enable = true;
      #   # autoLogin = {
      #   #   enable = true;
      #   #   user = "d4hines";
      #   # };
      #   session = [
      #     {
      #       name = "custom-xmonad";
      #       manage = "desktop";
      #       start = ''
      #         ${pkgs.runtimeShell} /home/d4hines/xmonad-aarch64-linux &
      #         waitPID=$!
      #       '';
      #     }
      #   ];
      #   # defaultSession = "custom-xmonad";
      # };

      users = {
        mutableUsers = false;
        users."d4hines" = {
          isNormalUser = true;
          extraGroups = [ "wheel" "networkmanager" "video" "audio" ];
          openssh.authorizedKeys.keyFiles = [ ../../keys/authorized_keys ];
          hashedPassword = "$y$j9T$OuZP8HXAEu20L7iVWEAyK1$M6HZYVCQf1.nZFGzv9IstdZBjzlzdnc5S4lmSiBpUw7";
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

{ hardware-module, complice-xmobar }: {
  system = "aarch64-linux";
  modules =
    ((import ./services.nix) { inherit complice-xmobar; })
    ++ [
      hardware-module
      ({ pkgs, ... }:
        {
          fileSystems = {
            "/" = {
              device = "/dev/disk/by-label/NIXOS_SD";
              fsType = "ext4";
              options = [ "noatime" ];
            };
          };
          nix = {
            extraOptions = ''
              require-sigs = false
            '';
          };
          networking.hostName = "ARCTURUS";
          time.timeZone = "America/New_York";
          services.avahi.enable = true;
          services.avahi.nssmdns = true;
          # TODO: this doesn't seem to be working
          services.avahi.publish = {
            enable = true;
            domain = true;
            addresses = true;
          };
          environment.systemPackages = with pkgs; [ vim ];
          services.openssh = {
            enable = true;
            passwordAuthentication = false;
          };
          networking.firewall.allowedTCPPorts = [ 7000 ];
          users = {
            mutableUsers = false;
            users."d4hines" = {
              isNormalUser = true;
              hashedPassword = builtins.readFile ../../secrets/password;
              extraGroups = [ "wheel" ];
              openssh.authorizedKeys.keyFiles = [ ../../keys/authorized_keys ];
            };
          };
          programs.zsh.enable = true;
          security.sudo.wheelNeedsPassword = false;
        })
    ];
}

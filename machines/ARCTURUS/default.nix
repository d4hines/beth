{ hardware-module }: {
  system = "aarch64-linux";
  modules =
    (import ./services.nix)
    ++ [
      hardware-module
      ../../modules/avahi.nix
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
          programs.tmux.enable = true;
          security.sudo.wheelNeedsPassword = false;
        })
    ];
}

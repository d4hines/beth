{
  agenix,
  all-overlays,
  home,
  fix-nixpkgs-path,
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
    fix-nixpkgs-path
    home.nixosModules.home-manager
    nixosModules.orb
    # My settings for orb
    (
      { pkgs, ... }:
      {
        nix = {
          package = pkgs.nixVersions.stable;

          extraOptions = ''
            filter-syscalls = false
            experimental-features = nix-command flakes
            extra-platforms = aarch64-linux
          '';
          settings.trusted-users = [ "@wheel" ];
        };
        programs.zsh.enable = true;
        programs.git.lfs.enable = true;
        programs.nix-ld.enable = true;
        programs.nix-ld.libraries = [
          # Add any missing dynamic libraries for unpackaged programs
          # here, NOT in environment.systemPackages
        ];

        environment.systemPackages = with pkgs; [
          vim
          git
        ];
        users.users.d4hines = {
          uid = 501;
          extraGroups = [ "wheel" ];
          shell = pkgs.zsh;

          # simulate isNormalUser, but with an arbitrary UID
          isSystemUser = true;
          group = "users";
          createHome = true;
          home = "/home/dhines";
          homeMode = "700";
          useDefaultShell = true;
        };
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

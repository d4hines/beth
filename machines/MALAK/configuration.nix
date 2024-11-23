{ pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    with pkgs; [
      vim
    ];
  environment.variables = {
    DEFAULT_USER = "dhines";
  };

  system.defaults.WindowManager.EnableStandardClickToShowDesktop = false;
  system.defaults.NSGlobalDomain.NSAutomaticWindowAnimationsEnabled = false;
  system.defaults.dock.launchanim = false;
  system.defaults.spaces.spans-displays = false;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # services.karabiner-elements.enable = true;
  homebrew = {
    enable = true;
    casks = [
      "nikitabobko/tap/aerospace"
      "flameshot"
    ];
    brews = [
      "borders"
    ];
    taps = [
      "FelixKratz/formulae" # to provide borders
    ];
  };
  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # nix.package = pkgs.nix;

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";

  users.users.dhines = {
    name = "dhines";
    home = "/Users/dhines";
  };
}

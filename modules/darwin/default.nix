{ pkgs, ... }:
{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim
  ];
  system.defaults.WindowManager.EnableStandardClickToShowDesktop = false;
  system.defaults.WindowManager.StandardHideDesktopIcons = true;
  system.defaults.NSGlobalDomain.NSAutomaticWindowAnimationsEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticPeriodSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
  system.defaults.spaces.spans-displays = false;
  system.defaults.dock.launchanim = false;
  system.defaults.dock.autohide = true;
  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.AppleShowAllFiles = true;

  # Auto upgrade nix package and the daemon service.
  nix.enable = true;

  # services.karabiner-elements.enable = true;
  homebrew = {
    enable = true;
    casks = [
      "nikitabobko/tap/aerospace"
      "notunes"
    ];
    brews = [
      "borders"
      # to enable touch id in tmux, add this line to /etc/pam.d/sudo
      # auth     optional     /opt/homebrew/lib/pam/pam_reattach.so
      "pam-reattach"
      "terminal-notifier"
    ];
    taps = [
      "FelixKratz/formulae" # to provide borders
    ];
  };

  launchd.user.agents."flameshot" = {
    command = "${pkgs.flameshot}/bin/flameshot";
    serviceConfig = {
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = "/tmp/flameshot.log";
      StandardErrorPath = "/tmp/flameshot.error.log";
    };
  };

  launchd.user.agents."macos-notification-server" = {
    command = "${pkgs.macos-notification-server}/bin/macos-notification-server";
    serviceConfig = {
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = "/tmp/macos-notification-server.log";
      StandardErrorPath = "/tmp/macos-notification-server.error.log";
    };
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
  security.pam.services.sudo_local.touchIdAuth = true;
}

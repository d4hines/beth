# Adapted with thanks from https://github.com/Arcanyx-org/NiXium/blob/35c58621e4c78d63a56b521aca6a4f4795756750/src/nixos/users/modules/web-browsers/firefox/firefox.nix
{ lib, ... }:
let
  inherit (lib) mkDefault mkForce;
in
{
  programs.firefox = {
    policies = {
      AppAutoUpdate = mkForce false; # Disable automatic application update
      BackgroundAppUpdate = mkForce false; # Disable automatic application update in the background, when the application is not running.
      BlockAboutConfig = mkDefault true;
      BlockAboutProfiles = true;
      BlockAboutAddons = mkDefault true;
      DisableAppUpdate = mkForce true;
      DisableSystemAddonUpdate = mkForce true; # Do not allow addon updates
      DisableFeedbackCommands = mkForce true;
      DisableFirefoxStudies = mkForce true;
      DisableFirefoxAccounts = true; # Disable Firefox Sync
      DisablePocket = mkForce true; # Privacy Liability
      DisableTelemetry = mkForce true;
      DontCheckDefaultBrowser = mkForce true; # Stop being attention whore
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
        EmailTracking = true;
        # Exceptions = ["https://example.com"]
      };
      UserMessaging = {
        ExtensionRecommendations = false; # Don’t recommend extensions while the user is visiting web pages
        FeatureRecommendations = false; # Don’t recommend browser features
        Locked = true; # Prevent the user from changing user messaging preferences
        MoreFromMozilla = false; # Don’t show the “More from Mozilla” section in Preferences
        SkipOnboarding = true; # Don’t show onboarding messages on the new tab page
        UrlbarInterventions = false; # Don’t offer suggestions in the URL bar
        WhatsNew = false; # Remove the “What’s New” icon and menuitem
      };
      SearchBar = "separate";
      SearchSuggestEnabled = false;
      SearchEngines = {
        Remove = [
          "Amazon.com"
          "Bing"
          # "Google"
        ];
        Default = "Google";
      };
      DisableFirefoxScreenshots = false;
      ShowHomeButton = true;
      PictureInPicture = {
        Enabled = true;
        Locked = true;
      };
      PromptForDownloadLocation = false;
      Homepage = {
        URL = "https://www.google.com";
        StartPage = "homepage"; # Opens homepage on startup
      };
      FirefoxHome = {
        Search = false;
        TopSites = true;
        SponsoredTopSites = false; # Fuck you
        Highlights = true;
        Pocket = false;
        SponsoredPocket = false; # Fuck you
        Snippets = false;
        Locked = true;
      };
      FirefoxSuggest = {
        WebSuggestions = false;
        SponsoredSuggestions = false; # Mozilla, Fuck You
        ImproveSuggest = false;
        Locked = true;
      };
      BlockAboutSupport = true;
    };
  };
}

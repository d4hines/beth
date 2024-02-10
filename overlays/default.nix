[
  (import ./chrome.nix)
  (import ./xmonad)
  (import ./signal-desktop.nix)
  (import ./signal-cli.nix)
  (import ./google-cloud-sdk.nix)
  (import ./toolbox)
  # my packages
  (final: prev: {
    patdiff = prev.patdiff.overrideAttrs (_: {
      postFixup = ''
        patchShebangs --build $out/bin/patdiff-git-wrapper
      '';
    });
    wta =
      prev.writeScriptBin "wta" (builtins.readFile ./scripts/wta);
    activate-chrome-tab = prev.writeShellApplication {
      name = "activate-chrome-tab";
      runtimeInputs = with prev; [curl jq];
      text = builtins.readFile ./scripts/activate-chrome-tab;
    };
    twitch-notification-daemon =
      prev.writeScriptBin "twitch-notification-daemon" (builtins.readFile ./scripts/twitch_notifications.ts);
    roam-backup =
      prev.writeScriptBin "roam_backup.js" (builtins.readFile ./scripts/roam_backup.js);
    log-hours =
      prev.writeScriptBin "log-hours" (builtins.readFile ./scripts/log-hours.js);
  })
]

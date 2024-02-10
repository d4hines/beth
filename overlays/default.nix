[
  (import ./chrome.nix)
  (import ./xmonad)
  (import ./signal-desktop.nix)
  (import ./signal-cli.nix)
  (import ./google-cloud-sdk.nix)
  (import ./toolbox)
  # my packages
  (final: prev: let
    {
    patdiff = prev.patdiff.overrideAttrs (_: {
      postFixup = ''
        patchShebangs --build $out/bin/patdiff-git-wrapper
      '';
    });
    clone-bare-for-worktrees = prev.writeScriptBin "clone-bare-for-worktrees" ./scripts/clone_bare_for_worktrees;
    wta = prev.writeScriptBin "wta" (builtins.readFile ./scripts/wta);
    activate-chrome-tab = prev.writeScriptBin "act.js" (builtins.readFile ./scripts/act.js);
    twitch-notification-daemon = prev.writeScriptBin "twitch-notification-daemon" (builtins.readFile ./scripts/twitch_notifications.ts);
    roam-backup = prev.writeScriptBin "roam_backup.js" (builtins.readFile ./scripts/roam_backup.js);
    log-hours = makeNodeScript "log-hours";
  })
]

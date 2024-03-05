[
  (import ./chrome.nix)
  (import ./xmonad)
  (import ./signal-desktop.nix)
  (import ./signal-cli.nix)
  (import ./google-cloud-sdk.nix)
  (import ./toolbox)
  # my packages
  (final: prev: let
    writeBunScript = name: path:
      prev.writeScriptBin name ''
        #!${prev.bun}/bin/bun --cwd ${./scripts}
        ${builtins.readFile path}
      '';
  in {
    patdiff = prev.patdiff.overrideAttrs (_: {
      postFixup = ''
        patchShebangs --build $out/bin/patdiff-git-wrapper
      '';
    });
    wta = writeBunScript "wta" ./scripts/wta.ts;
    activate-chrome-tab = prev.writeShellApplication {
      name = "activate-chrome-tab";
      runtimeInputs = with prev; [curl jq];
      text = builtins.readFile ./scripts/activate-chrome-tab;
    };
    twitch-notification-daemon = writeBunScript "twitch-notification-daemon" ./scripts/twitch_notifications.ts;
    roam-backup = writeBunScript "roam_backup.js" ./scripts/roam_backup.js;
    roam-recurring-tasks = prev.writeShellApplication {
      # TODO: add runtime inputs to writeBunScript?
      name = "roam-recurring-tasks";
      runtimeInputs = with prev; [curl bun];
      text = ''
        #!/usr/bin/env bash
        bun ${./scripts/roam_recurring_tasks.ts}
      '';
    };
    log-hours = writeBunScript "log-hours" ./scripts/log-hours.js;
  })
]

[
  (import ./chrome.nix)
  (import ./signal-desktop.nix)
  (import ./xmonad)
  # my packages
  (
    final: prev:
    let
      writeBunScript =
        name: path:
        prev.writeScriptBin name ''
          #!${prev.bun}/bin/bun --cwd ${./scripts}
          ${builtins.readFile path}
        '';
    in
    {
      patdiff = prev.patdiff.overrideAttrs (_: {
        postFixup = ''
          patchShebangs --build $out/bin/patdiff-git-wrapper
        '';
      });
      wta = writeBunScript "wta" ./scripts/wta.ts;
      vm = prev.writeShellApplication {
        name = "vm";
        runtimeInputs = [ ];
        text = builtins.readFile ./scripts/vm.sh;
      };
      activate-chrome-tab = prev.writeShellApplication {
        name = "activate-chrome-tab";
        runtimeInputs = with prev; [
          curl
          jq
        ];
        text = builtins.readFile ./scripts/activate-chrome-tab;
      };
      twitch-notification-daemon = writeBunScript "twitch-notification-daemon" ./scripts/twitch_notifications.ts;
      roam-backup = writeBunScript "roam_backup.js" ./scripts/roam_backup.js;
      roam-recurring-tasks = prev.writeShellApplication {
        # TODO: add runtime inputs to writeBunScript?
        name = "roam-recurring-tasks";
        runtimeInputs = with prev; [
          curl
          bun
        ];
        text = ''
          #!/usr/bin/env bash
          bun ${./scripts/roam_recurring_tasks.ts}
        '';
      };
      log-hours = writeBunScript "log-hours" ./scripts/log-hours.js;
      macos-notification-server = prev.writeShellApplication {
        name = "macos-notification-server";
        runtimeInputs = with prev; [
          terminal-notifier
          netcat
        ];
        text = builtins.readFile ./scripts/macos_notification_server.sh;
      };
      vm-notify = prev.writeShellApplication {
        name = "vm-notify";
        runtimeInputs = with prev; [
          netcat
        ];
        text = builtins.readFile ./scripts/vm_notify.sh;
      };
      # code2prompt = prev.code2prompt.overrideAttrs (_: {
      #   src = prev.fetchFromGitHub {
      #     owner = "mufeedvh";
      #     repo = "code2prompt";
      #     rev = "f5a63c4a9e4cdf312ad997d5a76c00e05b242437";
      #     sha256 = "sha256-DSB8Hspfx5lBruN5mJztjHIk6FIuYzi7gSJ23LDDwAk=";
      #   };
      #   postPatch = "true";
      # });
    }
  )
]

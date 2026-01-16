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
      claude-code = prev.claude-code.overrideAttrs (o: {
        version = "2.1.7";
        src = prev.fetchzip {
          url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${final.claude-code.version}.tgz";
          hash = "sha256-s/XPemwJYPUNFBgWo00VQ6W6eFIy44y9lFoRN0Duk9I=";
        };
      });
      claude-sandbox = prev.writeShellApplication {
        name = "claude-sandbox";
        runtimeInputs = with prev; [
          bubblewrap
          passt # provides pasta for network namespace
          util-linux # for unshare
          coreutils
          iproute2
          gnugrep
          socat # for port forwarding
          final.claude-code
          # Common development tools
          curl
          git
          nodejs
        ];
        text = builtins.readFile ./scripts/claude-sandbox.sh;
      };
      claude-sandbox-test = prev.writeShellApplication {
        name = "claude-sandbox-test";
        runtimeInputs = with prev; [
          final.claude-sandbox
          socat
        ];
        text = ''
          # Start a socat server on localhost to test network isolation
          TEST_PORT=19847
          socat TCP-LISTEN:$TEST_PORT,bind=127.0.0.1,fork EXEC:'echo SANDBOX_LEAK' 2>/dev/null &
          SOCAT_PID=$!
          trap 'kill $SOCAT_PID 2>/dev/null' EXIT

          # Give socat time to start
          sleep 0.2

          # Run the test with the port passed as env var
          SANDBOX_TEST_PORT=$TEST_PORT claude-sandbox -- bash ${./scripts/claude-sandbox-test.sh}
        '';
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
    }
  )
]

{ scripts, gh-stack } @ inputs: [
  (import ./chrome.nix)
  (import ./xmonad)
  (final: prev:
    let
      scripts = inputs.scripts.packages.x86_64-linux.scripts;
      makeNodeScript = name:
        prev.writeScriptBin name
          ''#!/usr/bin/env sh
      exec ${scripts}/lib/node_modules/scripts/${name} "$@"
    '';
      makeService = { Description, ExecStart }:
        {
          Unit = {
            inherit Description;
            PartOf = [ "default.target" ];
          };
          Install = { WantedBy = [ "default.target" ]; };
          Service = { inherit ExecStart; };
        };
    in
    {
      gh-stack = inputs.gh-stack.defaultPackage.x86_64-linux;
      preview = prev.writeScriptBin "preview" ../scripts/preview;
      toggle_pomodoro = prev.writeShellApplication {
        name = "toggle_pomodoro";
        text =
          "COMPLICE_AUTH_TOKEN=${builtins.readFile ../../../secrets/complice_api}\n"
          +
          builtins.readFile ../scripts/toggle_pomodoro;
        runtimeInputs = with prev; [ jq curl coreutils ];
      };
      activate-chrome-tab = makeNodeScript "act.js";
      twitch-notifications-service = makeService {
        Description = "Twitch notification daemon";
        ExecStart = "${scripts}/lib/node_modules/scripts/twitch_notifications.js ${prev.dunst}/bin/dunstify";
      };
      browser-whitelist-service = makeService {
        Description = "Browser Whitelist daemon";
        ExecStart = "${scripts}/lib/node_modules/scripts/browser_whitelist.js ${../scripts/whitelist.json}";
      };
      roam-backup = makeNodeScript "roam_backup.js";
    })
]

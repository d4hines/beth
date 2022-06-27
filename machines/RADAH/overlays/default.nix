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
      patdiff = prev.patdiff.overrideAttrs (_: {
        postFixup = ''
          patchShebangs --build $out/bin/patdiff-git-wrapper
        '';
      });
      gh-stack = inputs.gh-stack.defaultPackage.x86_64-linux;
      preview = prev.writeScriptBin "preview" ../scripts/preview;
      clone-bare-for-worktrees = prev.writeScriptBin "clone-bare-for-worktrees" ../scripts/clone_bare_for_worktrees;
      activate-chrome-tab = makeNodeScript "act.js";
      twitch-notifications-service = makeService {
        Description = "Twitch notification daemon";
        ExecStart = "${scripts}/lib/node_modules/scripts/twitch_notifications.js ${prev.dunst}/bin/dunstify";
      };
      roam-backup = makeNodeScript "roam_backup.js";
    })
]

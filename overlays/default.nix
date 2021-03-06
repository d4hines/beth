{ dream2nix } @ inputs: [
  (import ./chrome.nix)
  (import ./xmonad)
  (import ./google-cloud-sdk.nix)
  (final: prev:
    let
      dream2nix-lib = dream2nix.lib.init {
        pkgs = prev;
        config.projectRoot = ./scripts;
      };
      npmPackages =
        let
          outputs = (dream2nix-lib.makeOutputs {
            source = prev.nix-filter.filter {
              root = ./scripts;
            };
            settings =
              [{ subsystemInfo.nodejs = (builtins.substring 0 2 prev.nodejs.version); }];
          });
        in
        outputs.packages.scripts;
      makeNodeScript = name:
        prev.writeScriptBin name
          ''#!/usr/bin/env sh
            exec ${npmPackages}/lib/node_modules/scripts/${name} "$@"
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
      clone-bare-for-worktrees = prev.writeScriptBin "clone-bare-for-worktrees" ./scripts/clone_bare_for_worktrees;
      activate-chrome-tab = makeNodeScript "act.js";
      twitch-notifications-service = makeService {
        Description = "Twitch notification daemon";
        ExecStart = "${npmPackages}/lib/node_modules/scripts/twitch_notifications.js ${prev.dunst}/bin/dunstify";
      };
      roam-backup = makeNodeScript "roam_backup.js";
    })
]

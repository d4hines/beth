{dream2nix} @ inputs: [
  (import ./chrome.nix)
  (import ./xmonad)
  (import ./signal-desktop.nix)
  (import ./signal-cli.nix)
  (import ./google-cloud-sdk.nix)
  (import ./toolbox)
  # typical overlay stuff
  # (final: prev: {
  #   signal-desktop = prev.signal-desktop.overrideAttrs (old: let
  #     pname = "signal-desktop";
  #     version = "6.11.0";
  #     src = prev.fetchurl {
  #       url = "https://updates.signal.org/desktop/apt/pool/main/s/${pname}/${pname}_${version}_amd64.deb";
  #       hash = "sha256-lOc5W3XYEWwTJ9/U2cMTH931Qv2e7d+9vomnK9Dj1S0=";
  #     };
  #   in {
  #     inherit version src;
  #   });
  # })
  # my packages
  (final: prev: let
    dream2nix-lib = dream2nix.lib.init {
      pkgs = prev;
      config.projectRoot = ./scripts;
    };
    npmPackages = let
      outputs = dream2nix-lib.makeOutputs {
        source = prev.nix-filter.filter {
          root = ./scripts;
        };
        settings = [{subsystemInfo.nodejs = builtins.substring 0 2 prev.nodejs.version;}];
      };
    in
      outputs.packages.scripts;
    makeNodeScript = name:
      prev.writeScriptBin name
      ''        #!/usr/bin/env sh
                    exec ${npmPackages}/lib/node_modules/scripts/${name} "$@"
      '';
    roam-api = prev.writeScriptBin "roam-api" ''
      #!/bin/sh
      export PUPPETEER_EXECUTABLE_PATH=${prev.chromium.outPath}/bin/chromium
      export ROAM_API_GRAPH=d4hines
      exec ${npmPackages}/lib/node_modules/.bin/roam-api "$@"'';
  in {
    inherit roam-api;
    patdiff = prev.patdiff.overrideAttrs (_: {
      postFixup = ''
        patchShebangs --build $out/bin/patdiff-git-wrapper
      '';
    });
    clone-bare-for-worktrees = prev.writeScriptBin "clone-bare-for-worktrees" ./scripts/clone_bare_for_worktrees;
    wta = prev.writeScriptBin "wta" ''
      #/bin/sh
      exec ${./scripts/wta} "$@"
    '';
    activate-chrome-tab = makeNodeScript "act.js";
    twitch-notification-daemon = prev.writeScriptBin "twitch-notification-daemon" ''
      #!/bin/sh
      ${npmPackages}/lib/node_modules/scripts/twitch_notifications.js ${prev.dunst}/bin/dunstify
    '';
    roam-backup = makeNodeScript "roam_backup.js";
    tag-time = prev.writeScriptBin "tag-time" ''
      #!/bin/sh
      export DUNSTIFY=${prev.dunst}/bin/dunstify 
      export ROAM_API=${roam-api}/bin/roam-api
      exec ${npmPackages}/lib/node_modules/scripts/tagtime.js "$@"'';
    log-hours = makeNodeScript "log-hours";
  })
]

final: prev:
let
  nodeModules = prev.mkNodeModules {
    src = ./.;
    pname = "my-node-modules";
    version = "0.0.0";
    packageOverrides = { };
  };
  makeNodeScript = name: path:
    prev.writeScriptBin name
      ''#!/usr/bin/env sh
      export NODE_PATH="${nodeModules}/node_modules"
      exec ${prev.nodejs}/bin/node ${path} "$@"
    '';
  makeNodeService = { Description, ExecStart }:
    {
      Unit = {
        inherit Description;
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
      Service = {
        inherit ExecStart;
        Environment = ''NODE_PATH="${nodeModules}/node_modules"'';
        Restart = "always";
      };
    };
in
{
  preview = prev.writeScriptBin "preview" ./preview;
  activate-chrome-tab = makeNodeScript "act.js" ./act.js;
  complice-xmobar = prev.stdenv.mkDerivation {
    name = "complice-xmobar";
    src = ./.;
    buildPhase = "
    cp ${./complice.js} complice.js
    ";
    installPhase = "
      mkdir -p $out/app
      cp -r ${nodeModules}/node_modules $out/app/node_modules
      cp complice.js $out/app
    ";
  };
  twitch-notifications-service = makeNodeService {
    Description = "Twitch notification daemon";
    ExecStart = "${prev.nodejs}/bin/node ${./twitch_notifications.js} ${prev.dunst}/bin/dunstify";
  };
  browser-whitelist-service = makeNodeService {
    Description = "Browser Whitelist daemon";
    ExecStart = "${prev.nodejs}/bin/node ${./browser_whitelist.js} ${./whitelist.json}";
  };
  roam-backup = makeNodeScript "roam_backup.js" ./roam_backup.js;
}

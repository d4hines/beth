final: prev:
let
  nodeModules = prev.mkNodeModules {
    src = ./.;
    pname = "my-node-modules";
    version = "0.0.0";
    packageOverrides = { };
  };
in
{
  activate-chrome-tab = prev.writeScriptBin "act.js"
    ''#!/usr/bin/env sh
      NODE_PATH="${nodeModules}/node_modules" ${prev.nodejs}/bin/node ${./act.js} "$@"
    '';
  twitch-notifications = prev.writeScriptBin "twitch-notifications"
    ''#!/usr/bin/env sh
      NODE_PATH="${nodeModules}/node_modules" ${prev.nodejs}/bin/node ${./twitch_notifications.js} "$@"
    '';
}

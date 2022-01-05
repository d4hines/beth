final: prev:
let
  nodeModules = prev.mkNodeModules {
    src = ./scripts;
    pname = "my-node-modules";
    version = "0.0.0";
    packageOverrides = { };
  };
in
{
  activate-chrome-tab = prev.writeScriptBin "act.js"
    ''#!/usr/bin/env sh
      NODE_PATH="${nodeModules}/node_modules" ${prev.nodejs}/bin/node ${./scripts/act.js}
    '';
}

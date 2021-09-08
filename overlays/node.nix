final: prev:
let
  nodeModules = prev.mkNodeModules {
    src = ./.;
    pname = "my-node-modules";
    version = "0.0.0";
    packageOverrides = {};
  };
in
{
  my-nodejs = prev.writeScriptBin "node"
    ''#!/usr/bin/env sh
      NODE_PATH="${nodeModules}/node_modules" ${prev.nodejs}/bin/node "$@"
    '';
}

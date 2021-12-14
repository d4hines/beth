final: prev:
let
  nodeModules = prev.mkNodeModules {
    src = ./.;
    pname = "my-node-modules";
    version = "0.0.0";
    packageOverrides = {};
    # Puppeteer tries to automatically download chrome on install
    # and thus requires some coaxing to get to work in nix.
    # See https://github.com/justinwoo/puppeteer-node2nix
    extraEnvVars = {
      "PUPPETEER_SKIP_DOWNLOAD" = 1;
    };
  };
in
{
  my-nodejs = prev.writeScriptBin "node"
    ''#!/usr/bin/env sh
      export PUPPETEER_EXECUTABLE_PATH="${prev.google-chrome}/bin/google-chrome-stable"
      export NODE_PATH="${nodeModules}/node_modules"
      ${prev.nodejs}/bin/node "$@"
    '';
}

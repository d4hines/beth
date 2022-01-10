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
      NODE_PATH="${nodeModules}/node_modules" ${prev.nodejs}/bin/node ${path} "$@"
    '';
in
{
  activate-chrome-tab = makeNodeScript "act.js" ./act.js;
  twitch-notifications = makeNodeScript "twitch-notifications" ./twitch_notifications.js;
  browser-whitelist = makeNodeScript "browser-whitelist" ./browser_whitelist.js;
  complice-xmobar-server = prev.writeScriptBin "complice-xmobar" ''
    #/usr/bin/env sh

    docker run \
      --name complice \
      --rm \
      -p 7000:7000 \
      -v ${./complice.js}:/tmp/complice.js \
      -v ${nodeModules}/node_modules:/tmp/node_modules \
      -e COMPLICE_TOKEN=${builtins.readFile ../../secrets/complice_api} \
      node /tmp/complice.js
  '';
}

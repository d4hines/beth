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
  complice-xmobar-service = makeNodeService {
    Description = "Complice<->Xmobar integration server";
    ExecStart = ''
    docker run \
        --name complice \
        --rm \
        -p 7000:7000 \
        -v ${./complice.js}:/tmp/complice.js \
        -v ${nodeModules}/node_modules:/tmp/node_modules \
        -e COMPLICE_TOKEN=${builtins.readFile ../../secrets/complice_api} \
        node /tmp/complice.js  
    '';
  };
  twitch-notifications-service = makeNodeService {
    Description = "Twitch notification daemon";
    ExecStart = "${prev.nodejs}/bin/node ${./twitch_notifications.js} ${prev.dunst}/bin/dunstify";
  };
  browser-whitelist-service = makeNodeService {
    Description = "Browser Whitelist daemon";
    ExecStart = "${prev.nodejs}/bin/node ${./browser_whitelist.js} ${./whitelist.json}";
  };
}

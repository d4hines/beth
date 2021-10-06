final: prev: with prev; {
  brave = prev.writeScriptBin "brave"
    ''#!/usr/bin/env bash
      ${prev.brave}/bin/brave --remote-debugging-port=9222 "$@"
    '';
}

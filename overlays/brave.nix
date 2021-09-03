final: prev: with prev; {
  brave = prev.writeScriptBin "brave"
    ''#!/usr/bin/env zsh
      ${prev.brave}/bin/brave --remote-debugging-port=9222 "$@"
    '';
}

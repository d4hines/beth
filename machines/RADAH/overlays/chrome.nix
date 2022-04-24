final: prev: with prev; {
  my-google-chrome = prev.writeScriptBin "chrome"
    ''#!/usr/bin/env bash
      ${prev.google-chrome}/bin/google-chrome-stable --remote-debugging-port=9222 "$@"
    '';
  brave = prev.writeScriptBin "brave"
    ''#!/usr/bin/env bash
      ${prev.brave}/bin/brave --remote-debugging-port=9222 "$@"
    '';
}

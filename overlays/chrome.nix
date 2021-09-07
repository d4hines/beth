final: prev: with prev; {
  my-google-chrome = prev.writeScriptBin "chrome"
    ''#!/usr/bin/env zsh
      ${prev.google-chrome}/bin/google-chrome-stable --remote-debugging-port=9222 "$@"
    '';
}

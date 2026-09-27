#!/bin/sh
# Usage: toggle-app-workspace.sh WORKSPACE BUNDLE_ID APP_NAME
# If the app has a window on WORKSPACE, toggle between WORKSPACE and the previous
# workspace; otherwise launch it (dwmac.toml's on-window-detected moves it there).
PATH=/opt/homebrew/bin:$PATH
ws=$1 id=$2 app=$3

if [ "$(dwmac list-windows --workspace "$ws" --app-bundle-id "$id" --count)" -gt 0 ]; then
  if [ "$(dwmac list-workspaces --focused)" = "$ws" ]; then
    dwmac workspace-back-and-forth
  else
    dwmac workspace "$ws"
  fi
else
  open -a "$app"
fi

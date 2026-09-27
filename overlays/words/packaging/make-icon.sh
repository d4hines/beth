#!/usr/bin/env bash
# Rasterize assets/logo.svg into assets/AppIcon.icns (all required sizes).
# Requires ImageMagick (`magick`) and `iconutil` (built into macOS).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SVG="$ROOT/assets/logo.svg"
ISET="$(mktemp -d)/AppIcon.iconset"
mkdir -p "$ISET"

render() { magick -background none "$SVG" -resize "${1}x${1}" "$ISET/$2"; }

render 16   icon_16x16.png
render 32   icon_16x16@2x.png
render 32   icon_32x32.png
render 64   icon_32x32@2x.png
render 128  icon_128x128.png
render 256  icon_128x128@2x.png
render 256  icon_256x256.png
render 512  icon_256x256@2x.png
render 512  icon_512x512.png
render 1024 icon_512x512@2x.png

iconutil -c icns "$ISET" -o "$ROOT/assets/AppIcon.icns"
echo "✓ wrote assets/AppIcon.icns"

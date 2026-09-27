#!/usr/bin/env bash
# Build a release binary and assemble Words.app.
#
# The .app bundle gives the Accessibility permission a stable identity (so you
# grant it once) and lets the app run on its own without a terminal.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
APP="$ROOT/dist/Words.app"
cd "$ROOT"

echo "==> Building release binary"
cargo build --release

echo "==> Assembling $APP"
rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"
cp "$ROOT/packaging/Info.plist" "$APP/Contents/Info.plist"
cp "$ROOT/target/release/word-counter" "$APP/Contents/MacOS/word-counter"
chmod +x "$APP/Contents/MacOS/word-counter"
if [ -f "$ROOT/assets/AppIcon.icns" ]; then
  cp "$ROOT/assets/AppIcon.icns" "$APP/Contents/Resources/AppIcon.icns"
fi

# Prefer the stable self-signed identity (see make-cert.sh) so the Accessibility
# grant survives rebuilds; fall back to ad-hoc if it isn't set up.
IDENTITY="Word Counter Dev"
if security find-identity -v -p codesigning 2>/dev/null | grep -q "$IDENTITY"; then
  echo "==> Code signing with '$IDENTITY'"
  codesign --force --deep --sign "$IDENTITY" "$APP"
else
  echo "==> Ad-hoc code signing (run packaging/make-cert.sh for a stable grant)"
  codesign --force --deep --sign - "$APP"
fi

echo "==> Done: $APP"
echo "    Launch with:  open \"$APP\""
echo "    On first launch, grant Accessibility in"
echo "    System Settings › Privacy & Security › Accessibility."

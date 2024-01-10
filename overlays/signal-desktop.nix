final: prev:
with prev; {
  signal-desktop = prev.signal-desktop.overrideAttrs (_: let
    version = "6.43.1";
  in {
    inherit version;
    src = fetchurl {
      url = "https://updates.signal.org/desktop/apt/pool/s/signal-desktop/signal-desktop_${version}_amd64.deb";
      hash = "sha256-mDxZFs+rI2eHkkvkmflras1WqBa/HBVBDpdk9NKaC2E=";
    };
  });
}

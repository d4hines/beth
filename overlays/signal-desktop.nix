final: prev: with prev; {
  signal-desktop = prev.signal-desktop.overrideAttrs (
    _:
    let
      version = "7.4.0";
    in
    {
      inherit version;
      src = fetchurl {
        url = "https://updates.signal.org/desktop/apt/pool/s/signal-desktop/signal-desktop_${version}_amd64.deb";
        hash = "sha256-9a8Y8ncatynKspC/q0YxUWJj+nENr1ArwCZA9Ng8Mxk=";
      };
    }
  );
}

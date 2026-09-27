{ rustPlatform, lib }:
rustPlatform.buildRustPackage {
  pname = "words";
  version = "0.1.0";
  src = lib.cleanSource ./.;
  cargoLock.lockFile = ./Cargo.lock;

  # Mirrors packaging/build-app.sh, minus signing: the store copy is ad-hoc
  # signed by the toolchain. The home-manager activation copies it to
  # ~/Applications and re-signs with the stable "Word Counter Dev" cert so the
  # Accessibility grant survives rebuilds.
  postInstall = ''
    app=$out/Applications/Words.app
    mkdir -p $app/Contents/MacOS $app/Contents/Resources
    cp packaging/Info.plist $app/Contents/Info.plist
    mv $out/bin/word-counter $app/Contents/MacOS/word-counter
    cp assets/AppIcon.icns $app/Contents/Resources/AppIcon.icns
    ln -s $app/Contents/MacOS/word-counter $out/bin/word-counter
  '';

  meta = {
    description = "macOS menu bar word and token counter";
    platforms = lib.platforms.darwin;
    mainProgram = "word-counter";
  };
}

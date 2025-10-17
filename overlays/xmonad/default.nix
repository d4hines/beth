final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        xmobar = super.xmobar.overrideAttrs (old: {
          configureFlags = old.configureFlags ++ [
            "-fwith_datezone"
          ];
        });
        my-xmonad =
          (prev.haskellPackages.callCabal2nix "my-xmonad" (prev.lib.sourceByRegex ./. [
            "xmonad.hs"
            "my-xmonad.cabal"
          ]) { }).overrideAttrs
            (o: {
              postInstall = ''
                mkdir -p $out/share/man/man1
                cp ${super.xmonad}/share/man/man1/xmonad.1.gz $out/share/man/man1/xmonad.1.gz
              '';
            });
      }
    );
  });
}

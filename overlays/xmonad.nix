final: prev: rec {
  haskellPackages = prev.haskellPackages.override (
    old: {
      overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (
          self: super: rec {
            xmobar = super.xmobar.overrideAttrs (
              old: {
                configureFlags = old.configureFlags ++ [
                  "-fwith_datezone"
                ];
              }
            );
            my-xmonad = self.callCabal2nix "my-xmonad"
              (
                prev.lib.sourceByRegex ../xmonad [
                  "xmonad.hs"
                  "xmobar.hs"
                  "my-xmonad.cabal"
                ]
              ) {};
          }
        );
    }
  );
}

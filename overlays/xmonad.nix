final: prev: rec {
  haskellPackages = prev.haskellPackages.override (
    old: {
      overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (
          self: super: rec {
            xmobar = super.xmobar.overrideAttrs (old: {
              version = "0.38";
              src = prev.fetchFromGitHub {
                owner = "jaor";
                repo = "xmobar";
                rev = "4bfad37993331e12b0915a1db0e4639b8931236b";
                sha256 = "dH4Jw2o77WsbXUTIdKTs304Z6CmwaAZb4MhQq9v9Mto=";
              };
            });
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

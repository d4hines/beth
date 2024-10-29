final: prev: rec {
  haskellPackages = prev.haskellPackages.override (
    old: {
      overrides =
        prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (
          self: super: rec {
            play-xmonad =
              prev.haskellPackages.callCabal2nix "xmonad"
              (
                prev.lib.sourceByRegex ./. [
                  "xmonad.hs"
                  "Server.hs"
                  "xmonad.cabal"
                ]
              )
              {};
          }
        );
    }
  );
}

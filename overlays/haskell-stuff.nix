final: prev: rec {
  haskellPackages = prev.haskellPackages.override (
    old: {
      overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
        (
          self: super: rec {
            xmobar = super.xmobar.overrideAttrs (
              old: {
                configureFlags = old.configureFlags ++ [
                  "-fwith_datezone"
                ];
              }
            );
            xmonad = self.xmonad_0_17_0;
          }
        );
    }
  );
}

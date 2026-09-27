[
  (import ./xmonad)
  # mergiraf: upgrade to 0.16.3 (Haskell support) with strict aliasing fix
  (final: prev: {
    mergiraf = prev.mergiraf.overrideAttrs (old: {
      version = "0.16.3";
      src = prev.fetchFromGitea {
        domain = "codeberg.org";
        owner = "mergiraf";
        repo = "mergiraf";
        tag = "v0.16.3";
        hash = "sha256-KlielG8XxOlS5Np8LZT+GMujWw/7EDOwsZHWVjneV3g=";
      };
      cargoDeps = prev.rustPlatform.fetchCargoVendor {
        inherit (final.mergiraf) src;
        hash = "sha256-F6YtOgcAR4fN33j7Ae4ixhTfNctUfgkV3t1I7XJzHHw=";
      };
      # Work around strict aliasing UB in tree-sitter's array.h macros.
      CFLAGS = "-fno-strict-aliasing";
    });
  })
  # my packages
  (
    final: prev:
    let
      writeBunScript =
        name: path:
        prev.writeScriptBin name ''
          #!${prev.bun}/bin/bun --cwd ${./scripts}
          ${builtins.readFile path}
        '';
    in
    {
      patdiff = prev.patdiff.overrideAttrs (_: {
        postFixup = ''
          patchShebangs --build $out/bin/patdiff-git-wrapper
        '';
      });
      wta = writeBunScript "wta" ./scripts/wta.ts;
      gc-repos = prev.writeShellApplication {
        name = "gc-repos";
        runtimeInputs = with prev; [
          git
          fzf
          coreutils
          gnugrep
        ];
        text = builtins.readFile ./scripts/gc-repos.sh;
      };
      vm = prev.writeShellApplication {
        name = "vm";
        runtimeInputs = [ ];
        text = ''#!/usr/bin/env bash
          ssh vm zsh
        '';
      };
    }
  )
]

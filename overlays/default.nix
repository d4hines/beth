{ gh-stack }: [
  (import ./chrome.nix)
  (import ./scripts)
  (import ./xmonad)
  (final: prev: {
    inherit gh-stack;
  })
]

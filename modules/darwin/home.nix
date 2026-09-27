{ beth-home }:
{
  config,
  pkgs,
  ...
}:
{
  imports = [ beth-home ];
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  home.packages = with pkgs; [
    tesseract4
    vm
  ];
  programs.home-manager.enable = true;

  # Words (overlays/words): install a real copy (not a store symlink) at a
  # stable path and sign it with the self-signed "Word Counter Dev" cert
  # (overlays/words/packaging/make-cert.sh), so the Accessibility grant and the
  # login item survive rebuilds. Only reinstalls when the package changes.
  home.activation.installWords = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    src=${pkgs.words}/Applications/Words.app
    dst="$HOME/Applications/Words.app"
    stamp="${config.xdg.stateHome}/words/installed-from"
    if [ "$(cat "$stamp" 2>/dev/null)" != "$src" ] || [ ! -d "$dst" ]; then
      run mkdir -p "$HOME/Applications" "$(dirname "$stamp")"
      running=$(/usr/bin/pgrep -x word-counter >/dev/null && echo 1 || true)
      [ -n "$running" ] && run /usr/bin/pkill -x word-counter || true
      run rm -rf "$dst"
      run cp -R "$src" "$dst"
      run chmod -R u+w "$dst"
      if ! run /usr/bin/codesign --force --deep --sign "Word Counter Dev" "$dst"; then
        warnEcho "Words: 'Word Counter Dev' cert unavailable; ad-hoc signing (re-grant Accessibility after updates)"
        run /usr/bin/codesign --force --deep --sign - "$dst"
      fi
      run --silence sh -c "echo '$src' > '$stamp'"
      [ -n "$running" ] && run /usr/bin/open "$dst" || true
    fi
  '';
}

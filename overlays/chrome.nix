final: prev: with prev; {
  google-chrome = prev.google-chrome.override { commandLineArgs = "--remote-debugging-port=9222"; };
  chromium = prev.chromium.override { commandLineArgs = "--remote-debugging-port=9222"; };
}

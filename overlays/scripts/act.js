#!/usr/bin/env node

// Idempotently opens a browser tab and switches to it.
// Inspired by https://itectec.com/unixlinux/way-to-activate-a-particular-tab-of-chrome-via-bash/
// Only works if remote debugging port is set properly.
const { List, Activate } = require("chrome-remote-interface");
const { exec } = require("child_process");

const targetURL = process.argv[2];

(async () => {
  const targetTab = (await List()).find(
    ({ url, type }) => url.includes(targetURL) && type === "page"
  );
  console.log(targetURL, targetTab);
  if (targetTab) {
    await Activate({ id: targetTab.id });
  } else {
    exec(`chrome ${targetURL}`);
    process.exit(0);
  }
})();

#!/usr/bin/env node

// This script is used to keep me veering off track
// by closing any tabs not in my whitelist during specific
// conditions, specifically:
// - If /tmp/ultra_focus exists
// - If it's not either Saturday or not from the hours of 3pm to 8pm.
//   (I try to relegate browsing/etc. to those hours and keep the rest
//    reserved for intentional work or rest).
//
// Depends on https://github.com/cyrus-and/chrome-remote-interface
// Install with `npm i -g chrome-remote-interface`

const { execSync } = require("child_process");

const { existsSync, rmSync, statSync } = require("fs");

let whitelist = [
  "github",
  "gitlab",
  "stackoverflow",
  "archlinux.org",
  "chrome://newtab",
  "youtube.com",
  "complice",
  "meet.google.com",
  "calendar.google.com",
  "gather.town",
  "nixos.org",
  "slack.com/client/T59LZHQ11",
  "hackmd.io",
  "sketch.systems",
  "nomadic-labs.com",
  "whimsical.com",
  "roamresearch.com"
];

const matchesWhiteList = (str) => whitelist.some((x) => str.includes(x));

setInterval(() => {
  try {
    // if make_focus_exception exists,
    // allow any tabs, but only for 5 minutes
    if (existsSync("/tmp/make_focus_exception")) {
      let modified = new Date(statSync("/tmp/make_focus_exception").mtime);
      if ((new Date() - modified) / (1000 * 60) > 5) {
        rmSync("/tmp/make_focus_exception");
      } else {
        return;
      }
    }
    let date = new Date();
    if (date.getDay() === 6) return;
    if (
      date.getHours() < 15 ||
      date.getHours() >= 20 ||
      existsSync("/tmp/ultra_focus")
    ) {
      let results = JSON.parse(
        execSync("chrome-remote-interface list").toString()
      )
        .filter((tab) => tab.type === "page")
        .filter((x) => !matchesWhiteList(x.url));

      for (const tab of results) {
        console.log(`Closing tab "${tab.title}"`);
        execSync(`chrome-remote-interface close "${tab.id}"`);
      }
    }
  } catch (error) {}
}, 1000);

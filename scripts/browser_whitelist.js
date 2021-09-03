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

const { existsSync, rmSync } = require("fs");

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
  "nixos.org"
];

const matchesWhiteList = (str) => whitelist.some((x) => str.includes(x));

let timeoutExists = false;
setInterval(() => {
  try {
    // if make_focus_exception exists,
    // allow any tabs, but only for 5 minutes
    if (existsSync("/tmp/make_focus_exception")) {
      if (!timeoutExists)
        setTimeout(() => {
          rmSync("/tmp/make_focus_exception");
        }, 1000 * 60 * 5);
      timeoutExists = true;
      return;
    }
    let date = new Date();
    if (date.getDate() === 6) return;
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

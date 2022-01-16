#!/usr/bin/env node

// This script is used to keep me veering off track
// by closing any tabs not in my whitelist during specific
// conditions, specifically:
// - If /tmp/ultra_focus exists
// - If it's not either Saturday or not from the hours of 3pm to 8pm.
//   (I try to relegate browsing/etc. to those hours and keep the rest
//    reserved for intentional work or rest).

const { Close, List } = require("chrome-remote-interface");
const { existsSync, rmSync, statSync, readFileSync } = require("fs");

process.title = "whitelist";

const whitelist_path = process.argv[2];

const matchesWhiteList = (whitelist, str) => whitelist.some((x) => str.includes(x));

console.log("Starting browser whitelist");

setInterval(async () => {
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
      let results = await List();
      const whitelist = JSON.parse(readFileSync(whitelist_path, "utf-8"));
      results = results
        .filter((tab) => tab.type === "page")
        .filter((x) => !matchesWhiteList(whitelist, x.url));
      for (const tab of results) {
        console.log(`Closing tab "${tab.title}"`);
        await Close({ id: tab.id });
      }
    }
  } catch (error) {
    console.error(error);
  }
}, 1000);

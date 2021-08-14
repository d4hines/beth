#!/usr/bin/env node

const fetch = require("node-fetch");
const token = Buffer.from(process.env.COMPLICE_TOKEN, "base64").toString(
  "ascii"
);
const { execSync } = require("child_process");
const complice = (x) => `https://complice.co/api/v0/${x}?auth_token=${token}`;
(async () => {
  // ----- Perform the grey-scale toggle if necessary; --------
  let timerState = await fetch(complice("u/me/today/timer/all")).then((x) =>
    x.json()
  );
  let greyScaleState = timerState.ticker.state == "inactive" ? "on" : "off";
  try {
    execSync(`./toggle_greyscale.sh ${greyScaleState}`);
  } catch (error) {}

  // Retrieve and output the current intention
  console.log("hello world");
})();

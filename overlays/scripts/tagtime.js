#!/usr/bin/env node

// From https://forum.beeminder.com/t/official-reference-implementation-of-the-tagtime-universal-ping-schedule/4282
const GAP = 45 * 60; // Average gap between pings, in seconds
const URPING = 1184097393; // Ur-ping ie the birth of Timepie/TagTime! (unixtime)
const SEED = 11193462; // Initial state of the random number generator
const IA = 16807; // =7^5: Multiplier for LCG random number generator
const IM = 2147483647; // =2^31-1: Modulus used for the RNG

// Above URPING is in 2007 and it's fine to jump to any later URPING/SEED pair
// like this one in 2018 -- URPING = 1532992625, SEED = 75570 -- without
// deviating from the universal ping schedule.

let pung = URPING; // Global var with unixtime (in seconds) of last computed ping
let state = SEED; // Global variable that's the state of the RNG

// Linear Congruential Generator, returns random integer in {1, ..., IM-1}.
// This is ran0 from Numerical Recipes and has a period of ~2 billion.
function lcg() {
  return (state = (IA * state) % IM);
} // lcg()/IM is a U(0,1) R.V.

// Return a random number drawn from an exponential distribution with mean m
function exprand(m) {
  return -m * Math.log(lcg() / IM);
}

// Every TagTime gap must be an integer number of seconds not less than 1
function gap() {
  return Math.max(1, Math.round(exprand(GAP)));
}

// Return unixtime of the next ping. First call init(t) and then call this in
// succession to get all the pings starting with the first one after time t.
function nextping() {
  return (pung += gap());
}

// Start at the beginning of time and walk forward till we hit the first ping
// strictly after time t. Then scooch the state back a step and return the first
// ping *before* (or equal to) t. Then we're ready to call nextping().
function init(t) {
  let p, s; // keep track of the previous values of the global variables
  [pung, state] = [URPING, SEED]; // reset the global state
  for (; pung <= t; nextping()) {
    [p, s] = [pung, state];
  } // walk forward
  [pung, state] = [p, s]; // rewind a step
  return pung; // return most recent ping time <= t
}

// const recentPing = init(new Date() / 1000);

// for (let i = 0; i < 10; i++) {
//   nextping();
//   console.log(pung, state);
// }

/////////////////////////////////////////////////////////////////////////////////////
const { execSync } = require("child_process");
const ROAM_API = process.env["ROAM_API"];
const DUNSTIFY = process.env["DUNSTIFY"];
const PUSHCUT_URL = process.env["PUSHCUT_URL"];

const recentPing = init(new Date() / 1000);
console.log("Ping initialized. Most recent ping and state:", recentPing, state);

const sleep = (ms) => {
  return new Promise((res) => setTimeout(res, ms));
};

function formatTime(date) {
  const minutes = date.getMinutes();
  const minutesStr = minutes >= 10 ? `${minutes}` : `0${minutes}`;
  const hours = date.getHours();
  const hoursStr = hours >= 10 ? `${hours}` : `0${hours}`;
  return `${hoursStr}:${minutesStr}`;
}

(async () => {
  while (true) {
    const nextPing = new Date(nextping() * 1000);
    await sleep(nextPing - new Date());
    const day = nextPing.getDay();
    const hour = nextPing.getHours();
    if (day !== 0 && hour > 6 && hour <= 20) {
      console.log("Ping at", nextPing);
      execSync(`${DUNSTIFY} TagTime Ping`);
      execSync(
        `${ROAM_API} create "{{[[TODO]]}} #tagtime ${formatTime(nextPing)}"`
      );
      await fetch(`${PUSHCUT_URL}/notifications/Tag%20Time`, {
        method: "POST",
      });
    } else {
      console.log("Skipping ping", nextPing);
    }
  }
})();

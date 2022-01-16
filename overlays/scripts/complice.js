#!/usr/bin/env node

const fetch = require("node-fetch");
const express = require("express");
const Color = require("color");
const fs = require("fs");
const { execSync } = require("child_process");

const DARK_GREY_COLOR = "#21252B";
const PINK_COLOR = "#e06c75";

const apiToken = Buffer.from(process.env.COMPLICE_TOKEN, "base64")
  .toString("ascii")
  .trim();

const callAPI = (endpoint) => {
  const url = `https://complice.co/api/v0/${endpoint}?auth_token=${apiToken}`;
  return fetch(url).then((x) => x.json());
};

const pastel = (color, amount) => Color(color).lighten(amount).hex();

const colorText = (text, color, backgroundColor) => {
  const str = backgroundColor
    ? `<fc=${color},${backgroundColor}>${text}</fc>`
    : `<fc=${color}>${text}</fc>`;
  return str.replace("\n", "");
};

let ticker;
let intentionText;
let goalNumber;
let color;
let grayscale = false;

function sayIntention() {
  if (!intentionText) {
    return "No intentions for today yet";
  } else {
    const backgroundColor = pastel(color, 0.8);
    const start = colorText("\ue0b0", DARK_GREY_COLOR, backgroundColor);
    const number = colorText(
      goalNumber == "x" ? " &) " : " " + goalNumber + ") ",
      color,
      backgroundColor
    );
    const text = colorText(intentionText + " ", color, backgroundColor);
    const left = `${start}${number}${text}`;
    const inactiveText =
      left + colorText("\ue0b0", backgroundColor, DARK_GREY_COLOR);
    if (ticker.state === "inactive") {
      return inactiveText;
    } else {
      const pomodoroBackgroundColor = pastel(PINK_COLOR, 0.4);
      const endTime = new Date(ticker.endTime - Date.now());
      const seconds =
        endTime.getSeconds() < 10
          ? "0" + endTime.getSeconds()
          : endTime.getSeconds();
      if (ticker.state !== "breaking" && endTime.getMinutes() > 25) {
        return inactiveText;
      } else {
        const timeRemaining = `${endTime.getMinutes()}:${seconds}`;
        const pomodoroStart = colorText(
          "\ue0b0",
          backgroundColor,
          pomodoroBackgroundColor
        );
        const pomodoroEnd = colorText("\ue0b0", pomodoroBackgroundColor);
        const timerText = colorText(
          `🍅 ${timeRemaining} `,
          PINK_COLOR,
          pomodoroBackgroundColor
        );
        return `${left}${pomodoroStart}${timerText}${pomodoroEnd}`;
      }
    }
  }
}

const app = express();
app.use(express.json());

app.post("/", (req, res) => {
  console.log("Received webhook");
  res.send("Got it, thanks!");
  let data = req.body;
  if (data?.eventKey?.startsWith("timer.pomo") && data.ticker) {
    ticker = data.ticker;
  }
  if (data?.nexa) {
    intentionText = data.nexa.t;
    goalNumber = data.nexa.code;
    color = data.colors.color;

    if (!intentionText || intentionText === "inactive") {
      grayscale = true;
    } else {
      grayscale = false;
    }
  }
  sayIntention();
});

app.get("/status", (req, res) => {
  console.log("Received status request");
  if (req.hostname === "localhost") {
    res.send(sayIntention());
  }
});

app.get("/grayscale", (req, res) => {
  console.log("Received grayscale request");
  res.send(grayscale);
});


console.log("Initializing Complice server.");
(async () => {
  ticker = (await callAPI("u/me/today/timer/all")).ticker;
  console.log("Got initial ticker");
  let intention = (await callAPI("u/me/today/full.json")).core.list.filter(
    (x) => !x.d && !x.nvm
  )[0];
  console.log("Got initial intention");
  if (intention) {
    let goals = (await callAPI("u/me/goals/active.json")).goals;
    console.log("Got initial goal");
    intentionText = intention.t;
    goalNumber = intention.code;
    const goal = goals.find((x) => x.code == goalNumber);
    color = goal?.color ?? "#A9A195";
  }
  const port = 7000;
  console.log("Listening on port ", port);
  app.listen(port);
})();

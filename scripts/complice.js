#!/usr/bin/env node

const fetch = require("node-fetch");
const express = require("express");
const fs = require("fs");
const { execSync } = require("child_process");

/**
 * Thanks to this awesome redditor for the picom grayscale scripts:
 *  https://www.reddit.com/r/archlinux/comments/j2tt7o/how_to_turn_display_grayscale/g79z3ii?utm_source=share&utm_medium=web2x&context=3
 */
const activateGrayscale = () => {
  if (!fs.existsSync("/tmp/grayscale")) {
    execSync("sleep 0.1");
    execSync("touch /tmp/grayscale");
    execSync(`
    picom -b --backend glx --glx-fshader-win "
        uniform sampler2D tex;
            void main() {
                vec4 c = texture2D(tex, gl_TexCoord[0].xy);
                float y = dot(c.rgb, vec3(0.299, 0.587, 0.114));
                gl_FragColor = vec4(y, y, y, 1.0);
            }"
    `);
  }
};

const deactivateGrayscale = () => {
  try {
    fs.unlinkSync("/tmp/grayscale");
  } catch {}
  try {
    execSync("killall picom", { stdio: "ignore" });
  } catch {}
};

// Toggle grayscale once at the beginning, as there might
// be state left over from the last session.
// If a timer is going, it will be instantly cleared.
deactivateGrayscale();
activateGrayscale();

const apiToken = Buffer.from(process.env.COMPLICE_TOKEN, "base64")
  .toString("ascii")
  .trim();

const callAPI = (endpoint) => {
  const url = `https://complice.co/api/v0/${endpoint}?auth_token=${apiToken}`;
  return fetch(url).then((x) => x.json());
};

const pastel = (color, amount) =>
  execSync(`pastel lighten ${amount} '${color}' | pastel format hex`)
    .toString("ascii")
    .trim();

const colorText = (text, color, backgroundColor) => {
  const str = backgroundColor
    ? `<fc=${color},${backgroundColor}>${text}</fc>`
    : `<fc=${color}>${text}</fc>`;
  return str.replace("\n", "");
};

let intention;
let goals;

setInterval(async () => {
  intention = (await callAPI("u/me/today/full.json")).core.list.filter(
    (x) => !x.d && !x.nvm
  )[0];
  goals = (await callAPI("u/me/goals/active.json")).goals;
}, 1000);

function sayIntention(ticker) {
  if (!intention) {
    activateGrayscale();
    console.log("No intentions for today yet");
  } else {
    const intentionText = intention.t;
    const goalNumber = intention.code;
    const goal = goals.find((x) => x.code == goalNumber);
    const color = goal?.color ?? "#A9A195";
    const backgroundColor = pastel(color, 0.5);
    const start = colorText(
      "\ue0b0",
      process.env.DARK_GREY_COLOR,
      backgroundColor
    );
    const number = colorText(
      goalNumber == "x" ? " &) " : " " + goalNumber + ") ",
      color,
      backgroundColor
    );
    const text = colorText(intentionText + " ", color, backgroundColor);
    const left = `${start}${number}${text}`;
    const doTimerInactive = () => {
      activateGrayscale();
      console.log(
        left + colorText("\ue0b0", backgroundColor, process.env.DARK_GREY_COLOR)
      );
    };
    if (ticker.state === "inactive") {
      doTimerInactive();
    } else {
      const pomodoroBackgroundColor = pastel(process.env.PINK_COLOR, 0.3);

      const endTime = new Date(ticker.endTime - Date.now());
      const seconds =
        endTime.getSeconds() < 10
          ? "0" + endTime.getSeconds()
          : endTime.getSeconds();
      if (!ticker.state === "breaking" && endTime.getMinutes() > 25) {
        doTimerInactive();
      } else {
        try {
          fs.unlinkSync("/tmp/grayscale");
        } catch {}
        try {
          execSync("killall picom", { stdio: "ignore" });
        } catch {}
        const timeLeft = `${endTime.getMinutes()}:${seconds}`;
        const pomodoroStart = colorText(
          "\ue0b0",
          backgroundColor,
          pomodoroBackgroundColor
        );
        const pomodoroEnd = colorText("\ue0b0", pomodoroBackgroundColor);
        const timerText = colorText(
          `🍅 ${timeLeft} `,
          process.env.PINK_COLOR,
          pomodoroBackgroundColor
        );
        console.log(`${left}${pomodoroStart}${timerText}${pomodoroEnd}`);
      }
    }
  }
}

const app = express();
app.use(express.json());
app.post("/", (req, res) => {
  let data = req.body;
  if (data?.eventKey?.startsWith("timer.pomo") && data.ticker) {
    ticker = data.ticker;
    sayIntention(data.ticker);
  }
  res.send("Got it, thanks!");
});

(async () => {
  ticker = (await callAPI("u/me/today/timer/all")).ticker;
  setInterval(async () => sayIntention(ticker), 1000);
  app.listen(7000);
})();

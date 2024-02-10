#! /usr/bin/env nix-shell
/*
#! nix-shell -i bun -p bun
*/

const fs = require("fs");

const command = process.argv[2];

const target = process.argv[3];

if (!target) {
  throw new Error("You must provide the log path");
}

const logPath = `/home/d4hines/work_logs/${target}`;

switch (command) {
  case "start":
    fs.appendFileSync(logPath, `start, ${new Date().toISOString()}\n`);
    console.log(`Logged start for ${target}`);
    break;
  case "stop":
    fs.appendFileSync(logPath, `stop, ${new Date().toISOString()}\n`);
    console.log(`Logged stop for ${target}`);
    break;
  case "stats":
    const log = fs
      .readFileSync(logPath, { encoding: "utf-8" })
      .trim()
      .split("\n")
      .map((x) => x.split(", "));

    const pairs = log.reduce(
      ([x, acc], curr) => {
        switch (curr[0]) {
          case "start":
            if ("start" in x) {
              throw new Error("protocol violation: unexpected start");
            }
            x["start"] = new Date(curr[1]);
            return [x, acc];
          case "stop":
            if (!("start" in x)) {
              throw new Error("protocol violation: unexpected stop");
            }
            x["stop"] = new Date(curr[1]);
            x["duration"] = Number(
              (
                (x["stop"].getTime() - x["start"].getTime()) /
                (1000 * 60 * 60)
              ).toFixed(3)
            );
            return [{}, [x, ...acc]];
          default:
            throw new Error(`unexpected tag: '${curr[0]}'`);
        }
      },
      [{}, []]
    )[1];

    const durations = pairs.map((x) => x.duration);
    const total = durations.reduce((sum, duration) => sum + duration, 0);

    const average = total / pairs.length;

    const max = Math.max(...durations);
    const min = Math.min(...durations);

    if (process.argv[4] === "--quiet") {
      console.log(total);
    } else {
      console.log(`Total: ${total}`);
      console.log(`Average: ${average}`);
      console.log(`Max: ${max}`);
      console.log(`Min: ${min}`);
    }

    break;

  default:
    console.error("Unexpected command: ", command);
    break;
}

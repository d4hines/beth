const tmi = require("tmi.js");
const { execFileSync, execSync } = require("child_process");

process.title = "twitch-notifications";

const notify_command = process.argv[2];
console.log("Using notify command: ", notify_command);

const client = new tmi.Client({
  channels: ["luckyghost"],
});

client.connect();

console.log("Connected to Twitch successfully");

client.on("message", (channel, tags, message, self) => {
  console.log(`Message received: ${tags["display-name"]}: ${message}`);
  execFileSync(notify_command, [
    "--hints=string:bgcolor:#9146FF",
    tags["display-name"],
    "--",
    message,
  ]);
});

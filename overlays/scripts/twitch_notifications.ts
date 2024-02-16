import tmi from "tmi.js";
import {execFileSync} from "child_process";

process.title = "twitch-notifications";

const notify_command = process.argv[2];
console.log("Using notify command: ", notify_command);

const client = new tmi.Client({
  channels: ["d4hines"],
});

client.connect();

console.log("Connected to Twitch successfully");

client.on("message", (channel: any, tags: Record<string, string>, message: string, self: any) => {
  console.log(`Message received: ${tags["display-name"]}: ${message}`);
  execFileSync(notify_command, [
    "--hints=string:bgcolor:#9146FF",
    tags["display-name"],
    "--",
    message,
  ]);
});

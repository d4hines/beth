const tmi = require('tmi.js');
const {execSync} = require("child_process");

process.title = "twitch-notifications";

const client = new tmi.Client({
	channels: [ 'luckyghost' ]
});

client.connect();

client.on('message', (channel, tags, message, self) => {
    execSync(`notify-send -a twitch '${tags["display-name"]}: ${message}'`);
});

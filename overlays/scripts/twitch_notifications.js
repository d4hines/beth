const tmi = require('tmi.js');
const {execSync} = require("child_process");

const client = new tmi.Client({
	channels: [ 'nefasqs' ]
});

client.connect();

client.on('message', (channel, tags, message, self) => {
    execSync(`notify-send -a twitch '${tags["display-name"]}: ${message}'`);
});

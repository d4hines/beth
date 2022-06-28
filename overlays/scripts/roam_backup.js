#!/usr/bin/env node

const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const { chdir } = require("process");

chdir(process.argv[2]);
console.log("Set current directory to ", process.cwd());

const files = fs
  .readdirSync(process.cwd())
  .filter((x) => x.match(/backup-d4hines.*/))
  .sort()
  .reverse();

if (files.length) {
  const most_recent = files.shift();
  console.log("Most recent file is ", most_recent);
  if (files.length) {
    for (const file of files) {
      console.log("Removing ", file);
      fs.unlinkSync(file);
    }
  }
  let backup_path = "backup.edn"
  if (fs.existsSync(backup_path)) {
    console.log("Deleting previous backup.edn");
    fs.unlinkSync(backup_path);
  }
  console.log("Renaming most recent file to backup.edn");
  fs.renameSync(most_recent, backup_path);
  execSync("git add backup.edn");
  execSync("git commit --allow-empty -m 'Update notes'");
  execSync("git push");
} else {
    console.log("No recent backups found");
}

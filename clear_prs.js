#!/usr/bin/env node

const {execSync} =  require("child_process");

const prs = JSON.parse(execSync(`gh pr list --author='@me' --search='is:closed [d4hines]' --json number,title`));

for (const pr of prs) {
    if(pr.title.includes("[d4hines]")) {
        const new_title = pr.title.replace("[d4hines]", "").trim();
        // console.log(`Old title: ${pr.title}, new title: ${new_title}`);
        execSync(`gh pr edit ${pr.number} --title='${new_title}'`)
    }
}

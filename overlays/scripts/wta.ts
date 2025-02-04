import { simpleGit } from "simple-git";
import { mkdir } from "fs/promises";

import path from "path";
import {spawn } from "child_process";

const git = simpleGit();

const getFzf = (args: [string]): Promise<string> => {
  return new Promise((res, _rej) => {
    const fzfProcess = spawn("fzf", ["--height", "40%"], {
      stdio: ["pipe", "pipe", process.stderr],
    });

    fzfProcess.stdin.write(args.join("\n"));
    fzfProcess.stdin.end();

    let fzfResult = "";
    fzfProcess.stdout.on("data", (data) => {
      fzfResult += data.toString();
    });

    fzfProcess.on("close", (code) => {
      res(fzfResult);
    });
  });
};

async function createWorktree(newBranchArg: string) {
  const branches = Object.values((await git.branch()).branches);

  const newBranch = await (async () => {
    if (newBranchArg) {
      return newBranchArg.trim();
    } else {
      const fzfResult = await getFzf(
        branches.map((x: any) => x.name satisfies string) as [string],
      );
      if (!fzfResult) {
        throw new Error("No branch selected");
      }
      return fzfResult
        .replace("*", "")
        .trim()
        .replace(/^remotes\/[^/]+\//, "");
    }
  })();

  const valid = /^[a-zA-Z0-9_/-]+$/;
  if (!valid.test(newBranch)) {
    throw new Error(`Invalid branch name '${newBranch}'`);
  }
  const gitRoot = await git.revparse("--show-toplevel");
  const localBranchExists = branches.some((x) => x.name === newBranch);
  if (!localBranchExists) {
    let created = false;
    for (const branch of branches) {
      if (branch.name.startsWith("remotes/")) {
        const [_, remote, ...nameParts] = branch.name.split("/");
        const name = nameParts.join("/");
        if (name === newBranch) {
          console.log(
            `Creating branch ${newBranch} to track ${remote}/${newBranch}`,
          );
          await git.branch([newBranch, `${remote}/${newBranch}`]);
          created = true;
          break;
        }
      }
    }
    if (!created) {
      console.log(`Creating new local branch ${newBranch}`);
      await git.branch([newBranch]);
    }
  }

  const dir = `${path.dirname(gitRoot)}/${newBranch}`;

  console.log("Creating worktree directory", dir);
  await mkdir(dir, { recursive: true });
  const result = Bun.spawnSync({
    cmd: ["git", "worktree", "add", dir, newBranch],
  });
  if (!result.success) {
    console.error(result.stderr?.toString());
  } else {
    console.log(`Branch ${newBranch} checked out at ${dir}`);
  }
}

(async () => {
  const arg = process.argv[2];
  await createWorktree(arg);
})();

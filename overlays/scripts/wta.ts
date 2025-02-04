import { simpleGit } from "simple-git";
import { mkdir } from "fs/promises";

import path from "path";
import { execSync } from "child_process";

const git = simpleGit();

async function createWorktree(newBranchArg: string) {
  const branches = Object.values((await git.branch()).branches);

  const newBranch = (() => {
    if (newBranchArg) {
      return newBranchArg.trim();
    } else {
      const fzfResult = execSync(
        `echo "${branches.map((x: any) => x.name).join("\n")}" | fzf --height 40%`,
      )
        .toString()
        .trim();
      if (!fzfResult) {
        throw new Error("No branch selected");
      }
      return fzfResult.trim().replace(/^remotes\/[^/]+\//, '');
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

const path = require("path");
const fs = require("fs");
const { execSync } = require("child_process");
const { builtinModules } = require("module");

const is_main_or_master = (p) => p === "main" || p === "master";

const getDirectories = (source) =>
  fs
    .readdirSync(source, { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map((dirent) => dirent.name);

function find_git_root(dir) {
  if (dir === "/") {
    throw new Error(
      "Could not find the root of the git worktree. Expected a sibling folder named 'main' or 'master'."
    );
  }
  const root = getDirectories(dir).find(is_main_or_master);
  if (root) {
    return dir;
  } else {
    return find_git_root(path.dirname(dir));
  }
}

function updateWorkspaceFile(root, f) {
  const workspace = fs
    .readdirSync(root)
    .find((x) => x.endsWith("code-workspace"));
  if (workspace) {
    console.log("Updating workspace");
    let wsPath = path.join(root, workspace);
    const json = JSON.parse(fs.readFileSync(wsPath, { encoding: "utf-8" }));
    fs.writeFileSync(wsPath, JSON.stringify(f(json)));
  }
}

module.exports = { find_git_root, updateWorkspaceFile };

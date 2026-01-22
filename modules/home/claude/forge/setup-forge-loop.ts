#!/usr/bin/env bun

/**
 * Forge Loop Setup Script
 * Creates state file for in-session Forge loop
 */

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";

// Template path - can be set externally via env var, falls back to script directory
const TEMPLATE_FILE =
  process.env.FORGE_TEMPLATE_FILE ??
  join(dirname(Bun.main), "forge-template.md");

interface Args {
  file: string;
  maxIterations: number;
  help: boolean;
}

function parseArgs(argv: string[]): Args {
  const args: Args = {
    file: "CLAUDE.md",
    maxIterations: 0,
    help: false,
  };

  let i = 0;
  while (i < argv.length) {
    const arg = argv[i];

    if (arg === "-h" || arg === "--help") {
      args.help = true;
      i++;
    } else if (arg === "--max-iterations") {
      if (!argv[i + 1]) {
        console.error("Error: --max-iterations requires a number argument");
        process.exit(1);
      }
      const num = parseInt(argv[i + 1], 10);
      if (isNaN(num) || num < 0) {
        console.error(
          `Error: --max-iterations must be a positive integer or 0, got: ${argv[i + 1]}`
        );
        process.exit(1);
      }
      args.maxIterations = num;
      i += 2;
    } else if (arg.startsWith("-")) {
      console.error(`Error: Unknown option: ${arg}`);
      console.error("   Use --help for usage information");
      process.exit(1);
    } else {
      // Positional argument = file path
      args.file = arg;
      i++;
    }
  }

  return args;
}

interface ParsedFile {
  task: string;
  verification: string;
}

function parseMarkdownFile(filePath: string): ParsedFile {
  if (!existsSync(filePath)) {
    console.error(`Error: File not found: ${filePath}`);
    process.exit(1);
  }

  const content = readFileSync(filePath, "utf-8");

  // Extract ## Task section
  const taskMatch = content.match(/^## Task\s*\n([\s\S]*?)(?=^## |\n*$)/m);
  if (!taskMatch) {
    console.error("Error: Could not find '## Task' section in file");
    console.error("   Expected format:");
    console.error("   ## Task");
    console.error("   Your task description here");
    console.error("");
    console.error("   ## Verification");
    console.error("   Your verification criteria here");
    process.exit(1);
  }

  // Extract ## Verification section
  const verificationMatch = content.match(/^## Verification\s*\n([\s\S]*?)(?=^## |\n*$)/m);
  if (!verificationMatch) {
    console.error("Error: Could not find '## Verification' section in file");
    console.error("   Expected format:");
    console.error("   ## Task");
    console.error("   Your task description here");
    console.error("");
    console.error("   ## Verification");
    console.error("   Your verification criteria here");
    process.exit(1);
  }

  return {
    task: taskMatch[1].trim(),
    verification: verificationMatch[1].trim(),
  };
}

function showHelp(): void {
  console.log(`Forge Loop - Autonomous work with verification

USAGE:
  /forge-loop [file] [--max-iterations N]

  Reads task from a markdown file (default: CLAUDE.md)

FILE FORMAT:
  ## Task
  Your task description here.

  ## Verification
  How to verify the task is complete.

OPTIONS:
  --max-iterations <n>   Maximum iterations before auto-stop (default: unlimited)
  -h, --help             Show this help message

EXAMPLES:
  /forge-loop                           # Uses ./CLAUDE.md
  /forge-loop ./my-task.md              # Uses specified file
  /forge-loop --max-iterations 10       # Uses ./CLAUDE.md with iteration limit

STOPPING:
  Output a <forge> exit tag, reach --max-iterations, or use /cancel-forge`);
}

function main(): void {
  const args = parseArgs(process.argv.slice(2));

  if (args.help) {
    showHelp();
    process.exit(0);
  }

  // Parse task file
  const { task, verification } = parseMarkdownFile(args.file);

  // Check template exists
  if (!existsSync(TEMPLATE_FILE)) {
    console.error(`Error: Template file not found: ${TEMPLATE_FILE}`);
    process.exit(1);
  }

  // Read template and substitute placeholders
  let prompt = readFileSync(TEMPLATE_FILE, "utf-8");
  prompt = prompt.replaceAll("{{TASK}}", task);
  prompt = prompt.replaceAll("{{VERIFICATION}}", verification);

  // Create state file
  mkdirSync(".claude", { recursive: true });

  const stateContent = `---
active: true
iteration: 1
max_iterations: ${args.maxIterations}
task: "${task.replace(/"/g, '\\"')}"
verification: "${verification.replace(/"/g, '\\"')}"
started_at: "${new Date().toISOString()}"
---

${prompt}`;

  writeFileSync(".claude/forge-loop.local.md", stateContent);

  // Output setup message
  const maxIterDisplay =
    args.maxIterations > 0 ? args.maxIterations.toString() : "unlimited";

  console.log(`Forge loop activated!

Iteration: 1
Max iterations: ${maxIterDisplay}
Task: ${task}
Verification: ${verification}

To monitor: head -20 .claude/forge-loop.local.md
To cancel:  /cancel-forge

═══════════════════════════════════════════════════════════════

${prompt}`);
}

main();

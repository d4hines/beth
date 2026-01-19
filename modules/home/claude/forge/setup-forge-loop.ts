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
  task: string | null;
  verification: string | null;
  maxIterations: number;
  help: boolean;
}

function parseArgs(argv: string[]): Args {
  const args: Args = {
    task: null,
    verification: null,
    maxIterations: 0,
    help: false,
  };

  let i = 0;
  while (i < argv.length) {
    const arg = argv[i];

    switch (arg) {
      case "-h":
      case "--help":
        args.help = true;
        i++;
        break;

      case "--task":
        if (!argv[i + 1]) {
          console.error("Error: --task requires a text argument");
          console.error("");
          console.error("   Example: --task 'Build a REST API for todos'");
          process.exit(1);
        }
        args.task = argv[i + 1];
        i += 2;
        break;

      case "--verification":
        if (!argv[i + 1]) {
          console.error("Error: --verification requires a text argument");
          console.error("");
          console.error(
            "   Example: --verification 'npm test passes with no failures'"
          );
          process.exit(1);
        }
        args.verification = argv[i + 1];
        i += 2;
        break;

      case "--max-iterations":
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
        break;

      default:
        console.error(`Error: Unknown option: ${arg}`);
        console.error("   Use --help for usage information");
        process.exit(1);
    }
  }

  return args;
}

function showHelp(): void {
  console.log(`Forge Loop - Autonomous work with verification

USAGE:
  /forge-loop --task 'TASK' --verification 'VERIFICATION' [OPTIONS]

REQUIRED:
  --task '<text>'          The task to complete (USE QUOTES for multi-word)
  --verification '<text>'  How to verify completion (USE QUOTES for multi-word)

OPTIONS:
  --max-iterations <n>     Maximum iterations before auto-stop (default: unlimited)
  -h, --help               Show this help message

DESCRIPTION:
  Starts a Forge loop in your CURRENT session. The stop hook prevents
  exit and feeds the prompt back until you output a completion status.

  Exit conditions:
    <forge>DONE</forge>              - Verification passes
    <forge>BLOCKED: [reason]</forge> - Genuinely blocked
    <forge>CONCERN: [info]</forge>   - Discovered something user should know
    <forge>DECLINE: [reason]</forge> - Decline the task in pre-flight

EXAMPLES:
  /forge-loop --task 'Build a todo API' --verification 'curl returns 200 from /todos'
  /forge-loop --task 'Fix the auth bug' --verification 'npm test passes' --max-iterations 20
  /forge-loop --task 'Refactor cache layer' --verification 'All tests green and no type errors'

STOPPING:
  Output one of the <forge> exit tags, reach --max-iterations, or use /cancel-forge

MONITORING:
  # View current iteration:
  grep '^iteration:' .claude/forge-loop.local.md

  # View full state:
  head -20 .claude/forge-loop.local.md`);
}

function main(): void {
  const args = parseArgs(process.argv.slice(2));

  if (args.help) {
    showHelp();
    process.exit(0);
  }

  // Validate required arguments
  if (!args.task) {
    console.error("Error: --task is required");
    console.error("");
    console.error(
      "   Example: /forge-loop --task 'Build a REST API' --verification 'tests pass'"
    );
    process.exit(1);
  }

  if (!args.verification) {
    console.error("Error: --verification is required");
    console.error("");
    console.error(
      "   Example: /forge-loop --task 'Build a REST API' --verification 'tests pass'"
    );
    process.exit(1);
  }

  // Check template exists
  if (!existsSync(TEMPLATE_FILE)) {
    console.error(`Error: Template file not found: ${TEMPLATE_FILE}`);
    process.exit(1);
  }

  // Read template and substitute placeholders
  let prompt = readFileSync(TEMPLATE_FILE, "utf-8");
  prompt = prompt.replaceAll("{{TASK}}", args.task);
  prompt = prompt.replaceAll("{{VERIFICATION}}", args.verification);

  // Create state file
  mkdirSync(".claude", { recursive: true });

  const stateContent = `---
active: true
iteration: 1
max_iterations: ${args.maxIterations}
task: "${args.task.replace(/"/g, '\\"')}"
verification: "${args.verification.replace(/"/g, '\\"')}"
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
Task: ${args.task}
Verification: ${args.verification}

The stop hook is now active. When you try to exit, the prompt will be
fed back to you. You'll see your previous work in files and git history.

Exit conditions:
  <forge>DONE</forge>              - Verification passes
  <forge>BLOCKED: [reason]</forge> - Genuinely blocked
  <forge>CONCERN: [info]</forge>   - Something I should know
  <forge>DECLINE: [reason]</forge> - Decline in pre-flight

To monitor: head -20 .claude/forge-loop.local.md
To cancel:  /cancel-forge

═══════════════════════════════════════════════════════════════

${prompt}`);
}

main();

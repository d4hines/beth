#!/usr/bin/env bun

/**
 * Forge Stop Hook
 * Prevents session exit when a forge-loop is active
 * Feeds Claude's output back as input to continue the loop
 */

import { existsSync, readFileSync, writeFileSync, unlinkSync } from "fs";
import { dirname, join } from "path";

const FORGE_STATE_FILE = ".claude/forge-loop.local.md";

// Template directory - set via env var or fall back to script directory
const TEMPLATE_DIR =
  process.env.FORGE_TEMPLATE_DIR ?? dirname(Bun.main);

const ARBITER_TEMPLATE_FILE = join(TEMPLATE_DIR, "arbiter-prompt.md");

interface HookInput {
  transcript_path: string;
}

interface TranscriptMessage {
  role: string;
  message: {
    content: Array<{ type: string; text?: string }>;
  };
}

interface ParsedState {
  iteration: number;
  maxIterations: number;
  promptText: string;
  task: string;
  verification: string;
}

type ExitCondition =
  | { type: "DONE" }
  | { type: "BLOCKED"; reason: string }
  | { type: "CONCERN"; info: string }
  | { type: "DECLINE"; reason: string }
  | { type: "REST"; status: string }
  | { type: "ARBITER_APPROVED" }
  | { type: "ARBITER_NEEDS_WORK"; feedback: string }
  | { type: "NONE" };

function warn(message: string): void {
  console.error(`Warning: Forge loop: ${message}`);
}

function stopAndCleanup(message?: string): never {
  if (message) {
    console.error(message);
  }
  if (existsSync(FORGE_STATE_FILE)) {
    unlinkSync(FORGE_STATE_FILE);
  }
  process.exit(0);
}

function parseStateFile(): ParsedState | null {
  if (!existsSync(FORGE_STATE_FILE)) {
    return null;
  }

  const content = readFileSync(FORGE_STATE_FILE, "utf-8");

  // Parse YAML frontmatter (between --- markers)
  const frontmatterMatch = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
  if (!frontmatterMatch) {
    warn("State file corrupted");
    warn(`   File: ${FORGE_STATE_FILE}`);
    warn("   Problem: Invalid frontmatter format");
    warn("   Forge loop is stopping. Run /forge-loop again to start fresh.");
    return null;
  }

  const [, frontmatter, promptText] = frontmatterMatch;

  // Extract iteration
  const iterationMatch = frontmatter.match(/^iteration:\s*(\d+)/m);
  if (!iterationMatch) {
    warn("State file corrupted");
    warn(`   File: ${FORGE_STATE_FILE}`);
    warn("   Problem: 'iteration' field not found");
    warn("   Forge loop is stopping. Run /forge-loop again to start fresh.");
    return null;
  }
  const iteration = parseInt(iterationMatch[1], 10);

  // Extract max_iterations
  const maxIterMatch = frontmatter.match(/^max_iterations:\s*(\d+)/m);
  if (!maxIterMatch) {
    warn("State file corrupted");
    warn(`   File: ${FORGE_STATE_FILE}`);
    warn("   Problem: 'max_iterations' field not found");
    warn("   Forge loop is stopping. Run /forge-loop again to start fresh.");
    return null;
  }
  const maxIterations = parseInt(maxIterMatch[1], 10);

  // Extract task
  const taskMatch = frontmatter.match(/^task:\s*"(.*)"/m);
  const task = taskMatch ? taskMatch[1].replace(/\\"/g, '"') : "";

  // Extract verification
  const verificationMatch = frontmatter.match(/^verification:\s*"(.*)"/m);
  const verification = verificationMatch
    ? verificationMatch[1].replace(/\\"/g, '"')
    : "";

  return { iteration, maxIterations, promptText: promptText.trim(), task, verification };
}

function getLastAssistantMessage(transcriptPath: string): string | null {
  if (!existsSync(transcriptPath)) {
    warn("Transcript file not found");
    warn(`   Expected: ${transcriptPath}`);
    return null;
  }

  const content = readFileSync(transcriptPath, "utf-8");
  const lines = content.trim().split("\n");

  // Find last assistant message (JSONL format)
  let lastAssistantLine: string | null = null;
  for (const line of lines) {
    if (line.includes('"role":"assistant"')) {
      lastAssistantLine = line;
    }
  }

  if (!lastAssistantLine) {
    warn("No assistant messages found in transcript");
    return null;
  }

  try {
    const parsed: TranscriptMessage = JSON.parse(lastAssistantLine);
    const textParts = parsed.message.content
      .filter((c) => c.type === "text" && c.text)
      .map((c) => c.text!);
    return textParts.join("\n");
  } catch (e) {
    warn("Failed to parse assistant message JSON");
    warn(`   Error: ${e}`);
    return null;
  }
}

function parseForgeTag(output: string): ExitCondition {
  // Check for arbiter tags first
  const arbiterMatch = output.match(/<arbiter>([\s\S]*?)<\/arbiter>/);
  if (arbiterMatch) {
    const arbiterContent = arbiterMatch[1].trim();
    if (arbiterContent === "APPROVED") {
      return { type: "ARBITER_APPROVED" };
    }
    if (arbiterContent.startsWith("NEEDS_WORK:")) {
      return {
        type: "ARBITER_NEEDS_WORK",
        feedback: arbiterContent.slice("NEEDS_WORK:".length).trim(),
      };
    }
  }

  // Extract content from <forge>...</forge> tags
  const match = output.match(/<forge>([\s\S]*?)<\/forge>/);
  if (!match) {
    return { type: "NONE" };
  }

  const content = match[1].trim();

  if (content === "DONE") {
    return { type: "DONE" };
  }

  if (content.startsWith("BLOCKED:")) {
    return { type: "BLOCKED", reason: content.slice("BLOCKED:".length).trim() };
  }

  if (content.startsWith("CONCERN:")) {
    return { type: "CONCERN", info: content.slice("CONCERN:".length).trim() };
  }

  if (content.startsWith("DECLINE:")) {
    return { type: "DECLINE", reason: content.slice("DECLINE:".length).trim() };
  }

  if (content.startsWith("REST:")) {
    return { type: "REST", status: content.slice("REST:".length).trim() };
  }

  // Unknown forge tag content - treat as no exit condition
  return { type: "NONE" };
}

function updateIteration(newIteration: number): void {
  const content = readFileSync(FORGE_STATE_FILE, "utf-8");
  const updated = content.replace(
    /^iteration:\s*\d+/m,
    `iteration: ${newIteration}`
  );
  writeFileSync(FORGE_STATE_FILE, updated);
}

function buildArbiterPrompt(task: string, verification: string): string {
  if (!existsSync(ARBITER_TEMPLATE_FILE)) {
    throw new Error(`Arbiter template not found: ${ARBITER_TEMPLATE_FILE}`);
  }

  let template = readFileSync(ARBITER_TEMPLATE_FILE, "utf-8");
  template = template.replaceAll("{{TASK}}", task);
  template = template.replaceAll("{{VERIFICATION}}", verification);
  return template;
}

function buildRestContinuationPrompt(
  promptText: string,
  restStatus: string,
  iteration: number
): string {
  return `## REST checkpoint acknowledged

You paused at iteration ${iteration} with status:
> ${restStatus}

Take a breath. When ready, continue from where you left off.

---

${promptText}`;
}

async function main(): Promise<void> {
  // Read hook input from stdin
  const stdinContent = await Bun.stdin.text();
  let hookInput: HookInput;

  try {
    hookInput = JSON.parse(stdinContent);
  } catch {
    // No valid input - allow exit
    process.exit(0);
    return; // Help TypeScript understand control flow
  }

  // Check if forge-loop is active
  const state = parseStateFile();
  if (!state) {
    // No active loop or corrupted - allow exit (cleanup already done in parseStateFile)
    if (existsSync(FORGE_STATE_FILE)) {
      unlinkSync(FORGE_STATE_FILE);
    }
    process.exit(0);
    return; // Help TypeScript understand control flow
  }

  // Check if max iterations reached
  if (state.maxIterations > 0 && state.iteration >= state.maxIterations) {
    console.log(`Forge loop: Max iterations (${state.maxIterations}) reached.`);
    stopAndCleanup();
  }

  // Get last assistant message
  const lastOutput = getLastAssistantMessage(hookInput.transcript_path);
  if (!lastOutput) {
    stopAndCleanup("   Forge loop is stopping.");
  }

  // Check for forge exit conditions
  const exitCondition = parseForgeTag(lastOutput!);

  switch (exitCondition.type) {
    case "ARBITER_APPROVED":
      console.log("Forge loop: Arbiter approved - task complete!");
      console.log(`   Completed at iteration ${state.iteration}`);
      stopAndCleanup();

    case "ARBITER_NEEDS_WORK": {
      // Arbiter found issues - continue loop with feedback
      const nextIter = state.iteration + 1;
      updateIteration(nextIter);

      const feedbackPrompt = `## Arbiter Feedback

The arbiter reviewed your work and found issues:

> ${exitCondition.feedback}

Please address this feedback and continue working. When verification passes, output \`<forge>DONE</forge>\` again for another review.

---

${state.promptText}`;

      const response = {
        decision: "block",
        reason: feedbackPrompt,
        systemMessage: `Forge iteration ${nextIter} (addressing arbiter feedback)`,
      };
      console.log(JSON.stringify(response));
      return;
    }

    case "DONE": {
      // Worker claims done - invoke arbiter review
      const nextIter = state.iteration + 1;
      updateIteration(nextIter);

      const arbiterPrompt = buildArbiterPrompt(state.task, state.verification);

      const response = {
        decision: "block",
        reason: arbiterPrompt,
        systemMessage: `Forge iteration ${nextIter} (arbiter review)`,
      };
      console.log(JSON.stringify(response));
      return;
    }

    case "REST": {
      // Intentional pause - continue loop with acknowledgment
      const nextIter = state.iteration + 1;
      updateIteration(nextIter);

      const restPrompt = buildRestContinuationPrompt(
        state.promptText,
        exitCondition.status,
        state.iteration
      );

      const response = {
        decision: "block",
        reason: restPrompt,
        systemMessage: `Forge iteration ${nextIter} (resumed from REST)`,
      };
      console.log(JSON.stringify(response));
      return;
    }

    case "BLOCKED":
      console.log("Forge loop: Blocked");
      console.log(`   Reason: ${exitCondition.reason}`);
      console.log(`   Iteration: ${state.iteration}`);
      stopAndCleanup();

    case "CONCERN":
      console.log("Forge loop: Concern raised");
      console.log(`   Info: ${exitCondition.info}`);
      console.log(`   Iteration: ${state.iteration}`);
      stopAndCleanup();

    case "DECLINE":
      console.log("Forge loop: Task declined");
      console.log(`   Reason: ${exitCondition.reason}`);
      console.log(`   Iteration: ${state.iteration}`);
      stopAndCleanup();

    case "NONE":
      // Continue loop
      break;
  }

  // Not complete - continue loop with SAME PROMPT
  const nextIteration = state.iteration + 1;
  updateIteration(nextIteration);

  // Build system message with iteration count
  const systemMsg = `Forge iteration ${nextIteration} | Exit: <forge>DONE</forge>, <forge>BLOCKED: reason</forge>, <forge>CONCERN: info</forge>`;

  // Output JSON to block the stop and feed prompt back
  const response = {
    decision: "block",
    reason: state.promptText,
    systemMessage: systemMsg,
  };

  console.log(JSON.stringify(response));
}

main();

#!/usr/bin/env bun

/**
 * Forge Stop Hook
 * Prevents session exit when a forge-loop is active
 * Feeds Claude's output back as input to continue the loop
 */

import { existsSync, readFileSync, writeFileSync, unlinkSync } from "fs";

const FORGE_STATE_FILE = ".claude/forge-loop.local.md";

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
}

type ExitCondition =
  | { type: "DONE" }
  | { type: "BLOCKED"; reason: string }
  | { type: "CONCERN"; info: string }
  | { type: "DECLINE"; reason: string }
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

  return { iteration, maxIterations, promptText: promptText.trim() };
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

async function main(): Promise<void> {
  // Read hook input from stdin
  const stdinContent = await Bun.stdin.text();
  let hookInput: HookInput;

  try {
    hookInput = JSON.parse(stdinContent);
  } catch {
    // No valid input - allow exit
    process.exit(0);
  }

  // Check if forge-loop is active
  const state = parseStateFile();
  if (!state) {
    // No active loop or corrupted - allow exit (cleanup already done in parseStateFile)
    if (existsSync(FORGE_STATE_FILE)) {
      unlinkSync(FORGE_STATE_FILE);
    }
    process.exit(0);
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
  const exitCondition = parseForgeTag(lastOutput);

  switch (exitCondition.type) {
    case "DONE":
      console.log("Forge loop: Task completed successfully!");
      console.log(
        `   <forge>DONE</forge> detected at iteration ${state.iteration}`
      );
      stopAndCleanup();
      break;

    case "BLOCKED":
      console.log("Forge loop: Blocked");
      console.log(`   Reason: ${exitCondition.reason}`);
      console.log(`   Iteration: ${state.iteration}`);
      stopAndCleanup();
      break;

    case "CONCERN":
      console.log("Forge loop: Concern raised");
      console.log(`   Info: ${exitCondition.info}`);
      console.log(`   Iteration: ${state.iteration}`);
      stopAndCleanup();
      break;

    case "DECLINE":
      console.log("Forge loop: Task declined");
      console.log(`   Reason: ${exitCondition.reason}`);
      console.log(`   Iteration: ${state.iteration}`);
      stopAndCleanup();
      break;

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

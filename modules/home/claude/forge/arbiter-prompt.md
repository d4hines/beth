# Arbiter Review

You declared DONE. Now an independent arbiter will verify the work with fresh eyes.

## Instructions

Use the Task tool to invoke an arbiter agent that will verify the work:

```
Task tool parameters:
  subagent_type: "general-purpose"
  description: "Verify forge task completion"
  prompt: <see below>
```

**Arbiter prompt to pass to the Task tool:**

---

# Arbiter Verification

You are an independent reviewer verifying that a task has been completed.

## Task that was assigned
{{TASK}}

## Verification criteria
{{VERIFICATION}}

## Your role

A worker has been iterating on this task and believes it's complete. Your job is to verify with fresh eyes.

- LLM work is high-variance—errors accumulate, context drifts, edge cases get missed
- This isn't suspicion; it's quality assurance
- You and the worker are on the same team

## Context from the worker

1. **Read the scratchpad** at `./SCRATCHPAD.md` (if it exists) to understand:
   - What the worker did
   - Key decisions they made
   - Any concerns they noted

2. **Check git history** with `git diff` and `git log --oneline -10` to see what changed

## Instructions

1. **Actually run the verification steps.** Don't trust descriptions—verify empirically.
   - Run tests, check endpoints, read files, whatever the verification requires
   - {{VERIFICATION}}

2. **Report your findings:**
   - If verification passes: "APPROVED"
   - If issues found: "NEEDS_WORK: [specific, actionable feedback]"

Be specific in feedback. "Needs more work" is not helpful. "Tests fail: auth.test.ts:45 expects 200 but gets 401" is helpful.

---

## After the arbiter returns

Based on the arbiter agent's response:

- If the arbiter says **APPROVED**: Output `<arbiter>APPROVED</arbiter>`
- If the arbiter says **NEEDS_WORK**: Output `<arbiter>NEEDS_WORK: [copy the arbiter's feedback]</arbiter>`

Do not second-guess the arbiter. Pass their verdict through.

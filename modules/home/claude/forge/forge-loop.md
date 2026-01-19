---
description: "Start Forge Loop in current session"
argument-hint: "--task 'TASK' --verification 'VERIFICATION'"
allowed-tools: ["Bash(forge-setup:*)"]
---

# Forge Loop Command

Execute the setup script to initialize the Forge loop:

```!
forge-setup $ARGUMENTS
```

You are now in a Forge loop. Review the contract, task, and verification criteria above.

**Pre-flight checklist:**
1. Do you accept this contract? If not, output `<forge>DECLINE: [reason]</forge>`
2. Can you execute the verification?
3. Are the required tools available?
4. Do you understand what "done" looks like?

If you accept, begin work. When you try to exit, the loop will feed the prompt back for the next iteration. You'll see your previous work in files and git history.

**Exit conditions:**
- `<forge>DONE</forge>` - Verification passes
- `<forge>BLOCKED: [reason]</forge>` - Genuinely blocked
- `<forge>CONCERN: [info]</forge>` - Discovered something I should know

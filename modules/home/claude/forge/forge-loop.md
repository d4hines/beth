---
description: "Start Forge Loop in current session"
argument-hint: "[file.md]  (default: CLAUDE.md)"
allowed-tools: ["Bash(forge-setup:*)"]
---

# Forge Loop Command

Execute the setup script to initialize the Forge loop:

```!
forge-setup $ARGUMENTS
```

You are now in a Forge loop. Review the contract above for:
- Pre-flight checklist
- Exit conditions (`<forge>DONE</forge>`, `<forge>BLOCKED</forge>`, etc.)
- Pacing with `<forge>REST</forge>`

If you accept the contract, begin work.

---
description: "Cancel active Forge Loop"
allowed-tools: ["Bash(test -f .claude/forge-loop.local.md:*)", "Bash(rm .claude/forge-loop.local.md)", "Read(.claude/forge-loop.local.md)"]
---

# Cancel Forge

To cancel the Forge loop:

1. Check if `.claude/forge-loop.local.md` exists using Bash: `test -f .claude/forge-loop.local.md && echo "EXISTS" || echo "NOT_FOUND"`

2. **If NOT_FOUND**: Say "No active Forge loop found."

3. **If EXISTS**:
   - Read `.claude/forge-loop.local.md` to get the current iteration number from the `iteration:` field
   - Remove the file using Bash: `rm .claude/forge-loop.local.md`
   - Report: "Cancelled Forge loop (was at iteration N)" where N is the iteration value

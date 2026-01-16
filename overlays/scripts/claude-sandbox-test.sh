# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

pass() { echo -e "${GREEN}✓ PASS${NC}: $1"; }
fail() { echo -e "${RED}✗ FAIL${NC}: $1"; exit 1; }
info() { echo -e "${YELLOW}→${NC} $1"; }

echo "=== Claude Sandbox Test Suite ==="
echo

# Test 1: Nix store access
info "Test 1: Checking Nix store access..."
if [[ -d /nix/store ]] && ls /nix/store >/dev/null 2>&1; then
    # Try to find a common package
    if compgen -G "/nix/store/*-bash-*" > /dev/null; then
        pass "Nix store is accessible and contains packages"
    else
        pass "Nix store is accessible (no bash package found, but readable)"
    fi
else
    fail "Cannot access /nix/store"
fi

# Test 1b: ~/.claude directory exists and is writable
info "Test 1b: Checking \$HOME/.claude directory..."
if [[ -d "$HOME/.claude" ]]; then
    TEST_FILE="$HOME/.claude/.sandbox-test-$$"
    if touch "$TEST_FILE" 2>/dev/null && rm "$TEST_FILE" 2>/dev/null; then
        pass "\$HOME/.claude exists and is writable"
    else
        fail "\$HOME/.claude exists but is not writable"
    fi
else
    fail "\$HOME/.claude directory does not exist"
fi

# Test 1c: ~/repos directory exists and is writable
info "Test 1c: Checking \$HOME/repos directory..."
if [[ -d "$HOME/repos" ]]; then
    TEST_FILE="$HOME/repos/.sandbox-test-$$"
    if touch "$TEST_FILE" 2>/dev/null && rm "$TEST_FILE" 2>/dev/null; then
        pass "\$HOME/repos exists and is writable"
    else
        fail "\$HOME/repos exists but is not writable"
    fi
else
    fail "\$HOME/repos directory does not exist"
fi

# Test 2: Network access (internet)
info "Test 2: Checking internet access..."
if curl -s --max-time 10 https://api.github.com >/dev/null 2>&1; then
    pass "Internet access works (reached api.github.com)"
else
    fail "Cannot reach internet (api.github.com unreachable)"
fi

# Test 3: Host localhost isolation (using socat server started by wrapper)
info "Test 3: Checking host localhost isolation..."
if [[ -z "${SANDBOX_TEST_PORT:-}" ]]; then
    echo -e "${YELLOW}⊘ SKIP${NC}: SANDBOX_TEST_PORT not set (run via claude-sandbox-test wrapper)"
else
    # Try to connect to the socat server running on the host
    RESPONSE=$(curl -s --max-time 2 "http://127.0.0.1:$SANDBOX_TEST_PORT" 2>&1) || true
    if [[ "$RESPONSE" == *"SANDBOX_LEAK"* ]]; then
        fail "Host localhost is accessible (received response from host socat server!)"
    else
        pass "Host localhost is isolated (cannot reach host port $SANDBOX_TEST_PORT)"
    fi
fi

# Test 3b: SSH keys should be inaccessible
info "Test 3b: Checking SSH keys are inaccessible..."
if [[ -d "$HOME/.ssh" ]] || [[ -f "$HOME/.ssh/id_rsa" ]] || [[ -f "$HOME/.ssh/id_ed25519" ]]; then
    fail "SSH directory/keys are accessible (should be blocked!)"
else
    pass "SSH keys are inaccessible"
fi

# Test 3c: Home directory should not be fully accessible
info "Test 3c: Checking home directory isolation..."
# Check that we can't access common sensitive files
SENSITIVE_FILES=("$HOME/.bashrc" "$HOME/.zshrc" "$HOME/.aws/credentials" "$HOME/.gnupg")
ACCESSIBLE_COUNT=0
for f in "${SENSITIVE_FILES[@]}"; do
    if [[ -e "$f" ]]; then
        ((ACCESSIBLE_COUNT++)) || true
    fi
done
if [[ $ACCESSIBLE_COUNT -gt 0 ]]; then
    fail "Found $ACCESSIBLE_COUNT accessible sensitive files (home not isolated)"
else
    pass "Home directory properly isolated (sensitive files blocked)"
fi

# Test 4: npm install
info "Test 4: Checking npm install..."
TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"
cat > package.json <<'EOF'
{
  "name": "sandbox-test",
  "version": "1.0.0",
  "dependencies": {
    "is-odd": "^3.0.1"
  }
}
EOF

if npm install --silent 2>/dev/null; then
    if [[ -d node_modules/is-odd ]]; then
        pass "npm install works"
    else
        fail "npm install ran but node_modules missing"
    fi
else
    fail "npm install failed"
fi
cd - >/dev/null
rm -rf "$TEST_DIR"

# Test 5: Claude Code (using haiku for low token usage)
info "Test 5: Checking Claude Code access (haiku model)..."
if ! command -v claude >/dev/null 2>&1; then
    echo -e "${YELLOW}⊘ SKIP${NC}: claude not in PATH (install claude-code to test)"
else
    CLAUDE_RESPONSE=$(claude --model haiku --print "Reply with exactly: SANDBOX_TEST_OK" 2>&1) || true
    if [[ "$CLAUDE_RESPONSE" == *"SANDBOX_TEST_OK"* ]]; then
        pass "Claude Code works (haiku model responded)"
    else
        # Check if it's an API key issue vs sandbox issue
        if [[ "$CLAUDE_RESPONSE" == *"API"* ]] || [[ "$CLAUDE_RESPONSE" == *"key"* ]] || [[ "$CLAUDE_RESPONSE" == *"auth"* ]]; then
            fail "Claude Code API authentication failed (check ANTHROPIC_API_KEY)"
        else
            fail "Claude Code failed: $CLAUDE_RESPONSE"
        fi
    fi
fi

echo
echo -e "${GREEN}=== All tests passed! ===${NC}"

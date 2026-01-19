REPOS_DIR="${HOME}/repos"
NO_NET_ISOLATION=0
PORT_FORWARDS=()

usage() {
    cat <<EOF
Usage: claude-sandbox [OPTIONS] [-- COMMAND...]

Run Claude Code in a sandboxed environment with network isolation.

Options:
    -r, --repos DIR      Repos directory (default: \$HOME/repos)
    -L PORT              Forward host localhost:PORT to sandbox (like ssh -L)
    -L LOCAL:REMOTE      Forward host localhost:REMOTE to sandbox localhost:LOCAL
    --no-net-isolation   Disable network isolation (shares host network)
    -h, --help           Show this help

Examples:
    claude-sandbox
    claude-sandbox -r /path/to/repos
    claude-sandbox -L 5432                    # Forward postgres
    claude-sandbox -L 8080:80 -L 5432         # Multiple forwards
    claude-sandbox --no-net-isolation
EOF
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -r|--repos) REPOS_DIR="$2"; shift 2 ;;
        -L) PORT_FORWARDS+=("$2"); shift 2 ;;
        --no-net-isolation) NO_NET_ISOLATION=1; shift ;;
        -h|--help) usage; exit 0 ;;
        --) shift; break ;;
        *) echo "Unknown option: $1" >&2; exit 1 ;;
    esac
done

# Default command if none provided
if [[ $# -eq 0 ]]; then
    CMD=(claude --dangerously-skip-permissions)
else
    CMD=("$@")
fi

# Ensure directories exist
mkdir -p "$HOME/.claude"
mkdir -p "$REPOS_DIR"

# Find SSL cert path (resolve symlinks to get nix store path)
SSL_CERT=$(readlink -f /etc/ssl/certs/ca-certificates.crt 2>/dev/null || readlink -f /etc/ssl/certs/ca-bundle.crt 2>/dev/null || echo "")

# Create temp dir for sandbox files
SANDBOX_TMP=$(mktemp -d -t claude-sandbox.XXXXXX)
trap 'rm -rf "$SANDBOX_TMP"' EXIT

# Create sandbox .gitconfig (rewrites SSH to HTTPS, uses GH_TOKEN)
cat > "$SANDBOX_TMP/gitconfig" <<GITCFG
[url "https://github.com/"]
    insteadOf = git@github.com:
    insteadOf = ssh://git@github.com/
[url "https://gitlab.com/"]
    insteadOf = git@gitlab.com:
[credential]
    helper = "!f() { echo username=oauth; echo password=\$GH_TOKEN; }; f"
[user]
    name = ${GIT_AUTHOR_NAME:-$(git config user.name 2>/dev/null || echo "Claude Sandbox")}
    email = ${GIT_AUTHOR_EMAIL:-$(git config user.email 2>/dev/null || echo "sandbox@localhost")}
[safe]
    directory = *
GITCFG

# Get real UID/GID to preserve inside sandbox
REAL_UID=$(id -u)
REAL_GID=$(id -g)

# Find .env files in repos directory and block them with /dev/null
# This prevents accidental exposure of secrets to the AI agent
ENV_FILE_BLOCKS=()
while IFS= read -r -d '' envfile; do
    ENV_FILE_BLOCKS+=(--ro-bind /dev/null "$envfile")
done < <(find "$REPOS_DIR" -maxdepth 3 -type f \( -name ".env" -o -name ".env.*" -o -name "*.env" \) -print0 2>/dev/null || true)

# Build bwrap arguments
BWRAP_ARGS=(
    --unshare-user
    --unshare-pid
    --unshare-uts
    --uid "$REAL_UID"
    --gid "$REAL_GID"
    --die-with-parent
    --ro-bind /nix/store /nix/store
    --ro-bind /nix/var/nix /nix/var/nix
    --ro-bind /run/current-system /run/current-system
    --ro-bind /etc/profiles /etc/profiles
    --bind "$REPOS_DIR" "$REPOS_DIR"
    --bind "$HOME/.claude" "$HOME/.claude"
    --bind "$HOME/.claude.json" "$HOME/.claude.json"
    --bind /tmp /tmp
    --bind /bin /bin
    --dev /dev
    --proc /proc
    --ro-bind /etc/resolv.conf /etc/resolv.conf
    --ro-bind /etc/passwd /etc/passwd
    --ro-bind /etc/group /etc/group
    --ro-bind /etc/nsswitch.conf /etc/nsswitch.conf
    --ro-bind /etc/static /etc/static
    --ro-bind "$SANDBOX_TMP/gitconfig" "$HOME/.gitconfig"
    --setenv HOME "$HOME"
    --setenv USER "$USER"
    --setenv TERM "${TERM:-xterm-256color}"
    --setenv PATH "$PATH"
    --setenv GH_TOKEN "$(cat "$HOME/.gh_pat")"
    --setenv IS_SANDBOX 1
    --setenv SANDBOX_TEST_PORT "${SANDBOX_TEST_PORT:-}"
    --hostname claude-sandbox
    --chdir "$PWD"
    # Block .env files (overlay with /dev/null)
    "${ENV_FILE_BLOCKS[@]}"
)

# Add SSL cert env if found
if [[ -n "$SSL_CERT" ]]; then
    BWRAP_ARGS+=(--setenv SSL_CERT_FILE "$SSL_CERT")
    BWRAP_ARGS+=(--setenv NIX_SSL_CERT_FILE "$SSL_CERT")
fi

if [[ $NO_NET_ISOLATION -eq 0 ]]; then
    # Network isolation mode using pasta for both user namespace and networking
    # pasta creates a user namespace with proper mappings and sets up networking via DHCP

    # Remove user namespace options from bwrap since pasta handles that
    PASTA_BWRAP_ARGS=()
    skip_next=0
    for arg in "${BWRAP_ARGS[@]}"; do
        if [[ $skip_next -eq 1 ]]; then
            skip_next=0
            continue
        fi
        case "$arg" in
            --unshare-user) continue ;;
            --uid|--gid) skip_next=1; continue ;;
            *) PASTA_BWRAP_ARGS+=("$arg") ;;
        esac
    done

    # Build the forwarding wrapper script
    # 10.0.2.2 is mapped to host's localhost via pasta --map-host-loopback
    HOST_GATEWAY="10.0.2.2"
    if [[ ${#PORT_FORWARDS[@]} -gt 0 ]]; then
        # shellcheck disable=SC2016
        FORWARD_SCRIPT='cleanup() { kill $(jobs -p) 2>/dev/null; }; trap cleanup EXIT; '
        for fwd in "${PORT_FORWARDS[@]}"; do
            if [[ "$fwd" == *:* ]]; then
                LOCAL_PORT="${fwd%%:*}"
                REMOTE_PORT="${fwd##*:}"
            else
                LOCAL_PORT="$fwd"
                REMOTE_PORT="$fwd"
            fi
            FORWARD_SCRIPT+="socat TCP-LISTEN:$LOCAL_PORT,bind=127.0.0.1,fork,reuseaddr TCP:$HOST_GATEWAY:$REMOTE_PORT & "
        done
        FORWARD_SCRIPT+='exec "$@"'
    else
        FORWARD_SCRIPT='exec "$@"'
    fi

    # Use pasta to create user+network namespace with proper networking
    # pasta handles DHCP, DNS forwarding, and NAT automatically
    # --map-host-loopback makes HOST_GATEWAY refer to the host's localhost
    exec pasta --config-net -f --map-host-loopback "$HOST_GATEWAY" -- \
        bwrap "${PASTA_BWRAP_ARGS[@]}" -- bash -c "$FORWARD_SCRIPT" _ "${CMD[@]}"
else
    # No network isolation: share host network
    echo "Warning: Running without network isolation (--no-net-isolation)" >&2
    exec bwrap "${BWRAP_ARGS[@]}" -- "${CMD[@]}"
fi

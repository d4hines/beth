#!/usr/bin/env bash
# vm_notify.sh
# Script that sends notification messages to the macOS host via TCP

# Use localhost and the forwarded port
HOST="localhost"
PORT=54321

# Check if we have the required number of arguments
if [ $# -lt 2 ]; then
  echo "Usage: vm-notify <title> <message> [sound]"
  echo "  sound: optional sound name (default: none)"
  exit 1
fi

TITLE="$1"
MESSAGE="$2"
SOUND="${3:-none}"

# Send the notification data to the TCP port without any output
if ! echo "$TITLE|$MESSAGE|$SOUND" | nc -w 1 "$HOST" "$PORT" > /dev/null 2>&1; then
  echo "Error: Could not connect to notification server." >&2
  echo "Make sure the SSH connection with port forwarding is active." >&2
  exit 1
fi
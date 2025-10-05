#!/usr/bin/env bash
# macos_notification_server.sh
# Script that listens on a TCP port and forwards messages to terminal-notifier

# Use a high, random port number that's unlikely to be in use
PORT=54321

echo "Notification server listening on port $PORT"

# Use netcat to listen on the port
while true; do
  nc -l $PORT | while read -r line; do
    if [ -n "$line" ]; then
      # Extract title and message from the line
      title=$(echo "$line" | cut -d'|' -f1)
      message=$(echo "$line" | cut -d'|' -f2)
      sound=$(echo "$line" | cut -d'|' -f3)
      
      # Set default sound if not provided
      if [ -z "$sound" ] || [ "$sound" = "none" ]; then
        terminal-notifier -title "$title" -message "$message" &
      elif [ "$sound" = "custom" ]; then
        terminal-notifier -title "$title" -message "$message" &
        afplay "$HOME/done.wav" &
      else
        terminal-notifier -title "$title" -message "$message" -sound "$sound" &
      fi
    fi
  done
  
  # Small sleep to avoid high CPU usage in case of errors
  sleep 0.1
done
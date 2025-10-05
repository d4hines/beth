#!/usr/bin/env bash

# Port for notifications
PORT=54321

# Use ControlMaster to avoid reconnection issues
ssh -o ControlPath=~/.ssh/cm-%r@%h:%p \
    -o ControlMaster=auto \
    -o ControlPersist=10m \
    -o ExitOnForwardFailure=no \
    -R $PORT:localhost:$PORT -t vm zsh

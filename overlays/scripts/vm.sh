#!/usr/bin/env bash

# Use ControlMaster to avoid reconnection issues
ssh -o ControlPath=~/.ssh/cm-%r@%h:%p \
    -o ControlMaster=auto \
    -o ControlPersist=10m \
    -o ExitOnForwardFailure=no \
    -t vm zsh

#!/usr/bin/env bash

kitten ssh -o ControlPath=~/.ssh/cm-%r@%h:%p \
    -o ControlMaster=auto \
    -o ControlPersist=10m \
    -o ExitOnForwardFailure=no \
    -t vm zsh

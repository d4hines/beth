Bootstrap like so:

- git clone, then submodule init
- cd ./aconfmgr && ./aconfmgr restore ../arch-config
- reboot
- then nix
```
# TODO: install nix with nix-unstable-installer
# TODO: source nix env
nix-env --set-flag priority 6 $(nix-env -q | grep nix)
nix run github:nix-community/home-manager --no-write-lock-file switch --flake .#d4hines
```

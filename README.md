Bootstrap like so:

- git clone, then submodule init
- cd ./aconfmgr && ./aconfmgr restore ../arch-config
- reboot
- then nix
```
nix-env -iA nixpkgs.Unstable
nix run github:nix-community/home-manager --no-write-lock-file switch --flake .#d4hines
```

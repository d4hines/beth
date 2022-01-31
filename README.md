# My Dotfiles

This repo is a nix flake that produces:
- A NixOS configuration
- A home-manager configuration that I use from Arch Linux

Using the magic of Nix, I keep the two systems pretty in-sync.

## Structure

- `./flake.nix` imports the needed Nix libraries and glues everything together.
- `./modules/` houses config
- `./modules/home/` houses the home-manager config (which works isomorphically on Arch and NixOS)
- `./overlays/` provides additional Nix packages or modifies existing ones
- `./overlays/scripts/` houses a bunch of NodeJS scripts that I use to automate various things. Several have been packaged up as systemd services:
    - `browser-whitelist` automatically closes browser tabs that aren't on my whitelist during work hours to keep me on track. It's like https://freedom.to
       but... free.
    - `twitch-notifications` forwards messages from my [Twitch stream](https://twitch.tv/d4hines) to `notify-send`.
    - `complice-xmobar` helps display my https://complice.co tasks in Xmobar.
- `./keys` are my public keys.
- `./secrets` are my private keys and API tokens, encrypted automatically with [git-crypt](https://github.com/AGWA/git-crypt).

## Usage

On NixOS:
```
sudo nixos-rebuild --flake /path/to/repo#RADAH
```

On non-NixOS
```
home-manager switch --flake /path/to/repo#d4hines
```

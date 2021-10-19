AddPackageGroup xorg
AddPackageGroup xorg-drivers
# not sure why these removes are here but whatever
RemovePackage xorg-xauth
RemovePackage xorg-xdpyinfo
RemovePackage xorg-xmodmap
RemovePackage xorg-xrdb

CopyFile /etc/systemd/system/getty@tty1.service.d/override.conf

# I couldn't get the kitty nix package to work - OpenGL problems.
AddPackage kitty # A modern, hackable, featureful, OpenGL-based terminal emulator
AddPackage picom # X compositor that may fix tearing issues
AddPackage pulseaudio # A featureful, general-purpose sound server
CreateLink /etc/systemd/user/sockets.target.wants/pulseaudio.socket /usr/lib/systemd/user/pulseaudio.socket
CopyFile /etc/shells
AddPackage gnome-keyring # Stores passwords and encryption keys

AddPackage --foreign visual-studio-code-bin # Visual Studio Code (vscode)

AddPackage imagemagick # An image viewing/manipulation program

AddPackage noto-fonts-emoji # Google Noto emoji fonts
AddPackage --foreign mailspring # A beautiful, fast and maintained fork of Nylas Mail by one of the original authors.
AddPackage --foreign nerd-fonts-fira-code # Patched font Fira (Fura) Code from the nerd-fonts library

AddPackage --foreign logseq-desktop # A privacy-first, open-source platform for knowledge sharing and management.

AddPackage obs-studio # Free, open source software for live streaming and recording

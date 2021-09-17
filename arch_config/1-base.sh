
AddPackage base # Minimal package set to define a basic Arch Linux installation
AddPackage grub # GNU GRand Unified Bootloader (2)
AddPackage efibootmgr # Linux user-space application to modify the EFI Boot Manager
AddPackage networkmanager # Network connection manager and user applications
AddPackageGroup base-devel
AddPackage ntp # Network Time Protocol reference implementation
AddPackage openssh # Premier connectivity tool for remote login with the SSH protocol
AddPackage git # the fast distributed version control system
AddPackage linux # The Linux kernel and modules
AddPackage linux-firmware # Firmware files for Linux
AddPackageGroup linux-tools 
AddPackage vim # Vi Improved, a highly configurable, improved version of the vi text editor

AddPackage --foreign yay # Yet another yogurt. Pacman wrapper and AUR helper written in go.

CreateFile /etc/.pwd.lock 600 > /dev/null
CopyFile /etc/fstab
CopyFile /etc/locale.gen
CreateLink /etc/os-release ../usr/lib/os-release
CopyFile /etc/pacman.d/mirrorlist
CopyFile /etc/resolv.conf
CopyFile /etc/shells
CopyFile /etc/sudoers
CreateLink /etc/systemd/system/dbus-org.freedesktop.nm-dispatcher.service /usr/lib/systemd/system/NetworkManager-dispatcher.service
CreateLink /etc/systemd/system/getty.target.wants/getty@tty1.service /usr/lib/systemd/system/getty@.service
CreateLink /etc/systemd/system/multi-user.target.wants/NetworkManager.service /usr/lib/systemd/system/NetworkManager.service
CreateLink /etc/systemd/system/multi-user.target.wants/ntpdate.service /usr/lib/systemd/system/ntpdate.service
CreateLink /etc/systemd/system/multi-user.target.wants/remote-fs.target /usr/lib/systemd/system/remote-fs.target
CreateLink /etc/systemd/system/network-online.target.wants/NetworkManager-wait-online.service /usr/lib/systemd/system/NetworkManager-wait-online.service
CreateLink /etc/systemd/user/sockets.target.wants/dirmngr.socket /usr/lib/systemd/user/dirmngr.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-browser.socket /usr/lib/systemd/user/gpg-agent-browser.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-extra.socket /usr/lib/systemd/user/gpg-agent-extra.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-ssh.socket /usr/lib/systemd/user/gpg-agent-ssh.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent.socket /usr/lib/systemd/user/gpg-agent.socket
CreateLink /etc/systemd/user/sockets.target.wants/p11-kit-server.socket /usr/lib/systemd/user/p11-kit-server.socket
CreateDir /lost+found 700

# Mon Aug  9 16:59:53 UTC 2021 - New file properties

SetFileProperty /usr/bin/newgidmap mode 755
SetFileProperty /usr/bin/newuidmap mode 755
SetFileProperty /var/log/journal group systemd-journal



CopyFile /etc/pacman.d/mirrorlist.pacnew
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.alias
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.alias.bin
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.builtin.alias.bin
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.builtin.bin
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.dep
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.dep.bin
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.devname
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.softdep
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.symbols
CopyFile /usr/lib/modules/5.13.9-arch1-1/modules.symbols.bin

AddPackage git-crypt # Transparent file encryption in Git
AddPackage vi # The original ex/vi text editor

CopyFile /etc/shells

CreateLink /etc/systemd/system/sockets.target.wants/nix-daemon.socket /usr/lib/systemd/system/nix-daemon.socket

CopyFile /etc/nix/nix.conf

CopyFile /etc/hostname

CreateLink /etc/localtime ../usr/share/zoneinfo/America/New_York

CopyFile /etc/locale.conf

CopyFile /etc/hosts

# This might actually belong in ./2-tezos.sh, not sure.
AddPackage hidapi # Simple library for communicating with USB and Bluetooth HID devices
AddPackage libev # A full-featured and high-performance event loop
AddPackage libusb # Library that provides generic access to USB devices

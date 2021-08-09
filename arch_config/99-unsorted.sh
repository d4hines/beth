

# Mon Aug  9 16:59:53 UTC 2021 - Unknown packages


AddPackage autoconf # A GNU tool for automatically configuring source code
AddPackage automake # A GNU tool for automatically creating Makefiles
AddPackage base # Minimal package set to define a basic Arch Linux installation
AddPackage binutils # A set of programs to assemble and manipulate binary and object files
AddPackage bison # The GNU general-purpose parser generator
AddPackage bpf # BPF tools
AddPackage cgroup_event_listener # Simple listener of cgroup events
AddPackage cpupower # Linux kernel tool to examine and tune power saving related features of your processor
AddPackage efibootmgr # Linux user-space application to modify the EFI Boot Manager
AddPackage fakeroot # Tool for simulating superuser privileges
AddPackage file # File type identification utility
AddPackage findutils # GNU utilities to locate files
AddPackage flex # A tool for generating text-scanning programs
AddPackage gawk # GNU version of awk
AddPackage gcc # The GNU Compiler Collection - C and C++ frontends
AddPackage gettext # GNU internationalization library
AddPackage git # the fast distributed version control system
AddPackage grep # A string search utility
AddPackage groff # GNU troff text-formatting system
AddPackage grub # GNU GRand Unified Bootloader (2)
AddPackage gzip # GNU compression utility
AddPackage hidapi # Simple library for communicating with USB and Bluetooth HID devices
AddPackage hyperv # Hyper-V tools
AddPackage libev # A full-featured and high-performance event loop
AddPackage libtool # A generic library support script
AddPackage libusb # Library that provides generic access to USB devices
AddPackage linux # The Linux kernel and modules
AddPackage linux-firmware # Firmware files for Linux
AddPackage m4 # The GNU macro processor
AddPackage make # GNU make utility to maintain groups of programs
AddPackage networkmanager # Network connection manager and user applications
AddPackage ntp # Network Time Protocol reference implementation
AddPackage opam # OCaml package manager
AddPackage openssh # Premier connectivity tool for remote login with the SSH protocol
AddPackage pacman # A library-based package manager with dependency support
AddPackage patch # A utility to apply patch files to original sources
AddPackage perf # Linux kernel performance auditing tool
AddPackage pkgconf # Package compiler and linker metadata toolkit
AddPackage rust # Systems programming language focused on safety, speed and concurrency
AddPackage sed # GNU stream editor
AddPackage sudo # Give certain users the ability to run some commands as root
AddPackage texinfo # GNU documentation system for on-line information and printed output
AddPackage tmon # Monitoring and Testing Tool for Linux kernel thermal subsystem
AddPackage turbostat # Report processor frequency and idle statistics
AddPackage usbip # An USB device sharing system over IP network
AddPackage vim # Vi Improved, a highly configurable, improved version of the vi text editor
AddPackage which # A utility to show the full path of commands
AddPackage x86_energy_perf_policy # Read or write MSR_IA32_ENERGY_PERF_BIAS


# Mon Aug  9 16:59:53 UTC 2021 - Unknown foreign packages


AddPackage --foreign yay # Yet another yogurt. Pacman wrapper and AUR helper written in go.


# Mon Aug  9 16:59:53 UTC 2021 - New / changed files


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
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.alias
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.alias.bin
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.builtin.alias.bin
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.builtin.bin
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.dep
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.dep.bin
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.devname
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.softdep
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.symbols
CopyFile /usr/lib/modules/5.13.8-arch1-1/modules.symbols.bin


# Mon Aug  9 16:59:53 UTC 2021 - New file properties


SetFileProperty /usr/bin/newgidmap mode 755
SetFileProperty /usr/bin/newuidmap mode 755
SetFileProperty /var/log/journal group systemd-journal

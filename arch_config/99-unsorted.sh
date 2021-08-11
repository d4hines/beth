

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


# Tue Aug 10 11:22:37 AM UTC 2021 - Unknown packages


AddPackage kitty # A modern, hackable, featureful, OpenGL-based terminal emulator
AddPackage mesa-demos # Mesa demos and tools incl. glxinfo + glxgears
AddPackage xf86-input-evdev # X.org evdev input driver
AddPackage xf86-input-libinput # Generic input driver for the X.Org server based on libinput
AddPackage xf86-input-synaptics # Synaptics driver for notebook touchpads
AddPackage xf86-input-vmmouse # X.org VMWare Mouse input driver
AddPackage xf86-input-void # X.org void input driver
AddPackage xf86-video-amdgpu # X.org amdgpu video driver
AddPackage xf86-video-ati # X.org ati video driver
AddPackage xf86-video-dummy # X.org dummy video driver
AddPackage xf86-video-fbdev # X.org framebuffer video driver
AddPackage xf86-video-intel # X.org Intel i810/i830/i915/945G/G965+ video drivers
AddPackage xf86-video-nouveau # Open Source 3D acceleration driver for nVidia cards
AddPackage xf86-video-openchrome # X.Org Openchrome drivers
AddPackage xf86-video-qxl # Xorg X11 qxl video driver
AddPackage xf86-video-vesa # X.org vesa video driver
AddPackage xf86-video-vmware # X.org vmware video driver
AddPackage xf86-video-voodoo # X.org 3dfx Voodoo1/Voodoo2 2D video driver
AddPackage xorg-bdftopcf # Convert X font from Bitmap Distribution Format to Portable Compiled Format
AddPackage xorg-docs # X.org documentations
AddPackage xorg-font-util # X.Org font utilities
AddPackage xorg-fonts-100dpi # X.org 100dpi fonts
AddPackage xorg-fonts-75dpi # X.org 75dpi fonts
AddPackage xorg-fonts-encodings # X.org font encoding files
AddPackage xorg-iceauth # ICE authority file utility
AddPackage xorg-mkfontscale # Create an index of scalable font files for X
AddPackage xorg-server # Xorg X server
AddPackage xorg-server-common # Xorg server common files
AddPackage xorg-server-devel # Development files for the X.Org X server
AddPackage xorg-server-xephyr # A nested X server that runs as an X application
AddPackage xorg-server-xnest # A nested X server that runs as an X application
AddPackage xorg-server-xvfb # Virtual framebuffer X server
AddPackage xorg-sessreg # Register X sessions in system utmp/utmpx databases
AddPackage xorg-setxkbmap # Set the keyboard using the X Keyboard Extension
AddPackage xorg-smproxy # Allows X applications that do not support X11R6 session management to participate in an X11R6 session
AddPackage xorg-x11perf # Simple X server performance benchmarker
AddPackage xorg-xbacklight # RandR-based backlight control application
AddPackage xorg-xcmsdb # Device Color Characterization utility for X Color Management System
AddPackage xorg-xcursorgen # Create an X cursor file from PNG images
AddPackage xorg-xdriinfo # Query configuration information of DRI drivers
AddPackage xorg-xev # Print contents of X events
AddPackage xorg-xgamma # Alter a monitor's gamma correction
AddPackage xorg-xhost # Server access control program for X
AddPackage xorg-xinput # Small commandline tool to configure devices
AddPackage xorg-xkbcomp # X Keyboard description compiler
AddPackage xorg-xkbevd # XKB event daemon
AddPackage xorg-xkbutils # XKB utility demos
AddPackage xorg-xkill # Kill a client by its X resource
AddPackage xorg-xlsatoms # List interned atoms defined on server
AddPackage xorg-xlsclients # List client applications running on a display
AddPackage xorg-xpr # Print an X window dump from xwd
AddPackage xorg-xprop # Property displayer for X
AddPackage xorg-xrandr # Primitive command line interface to RandR extension
AddPackage xorg-xrefresh # Refresh all or part of an X screen
AddPackage xorg-xset # User preference utility for X
AddPackage xorg-xsetroot # Classic X utility to set your root window background to a given pattern or color
AddPackage xorg-xvinfo # Prints out the capabilities of any video adaptors associated with the display that are accessible through the X-Video extension
AddPackage xorg-xwayland # run X clients under wayland
AddPackage xorg-xwd # X Window System image dumping utility
AddPackage xorg-xwininfo # Command-line utility to print information about windows on an X server
AddPackage xorg-xwud # X Window System image undumping utility
AddPackage xterm # X Terminal Emulator


# Tue Aug 10 11:22:37 AM UTC 2021 - Unknown foreign packages


AddPackage --foreign visual-studio-code-bin # Visual Studio Code (vscode)


# Tue Aug 10 11:22:37 AM UTC 2021 - New / changed files


CreateLink /etc/systemd/system/sockets.target.wants/nix-daemon.socket /usr/lib/systemd/system/nix-daemon.socket


# Tue Aug 10 11:49:51 AM UTC 2021 - Unknown packages


AddPackage cronie # Daemon that runs specified programs at scheduled times and related tools


# Tue Aug 10 11:49:51 AM UTC 2021 - Missing packages


RemovePackage xterm


# Tue Aug 10 01:45:43 PM UTC 2021 - Unknown packages


CopyFile /etc/nix/nix.conf


# Wed Aug 11 03:00:23 PM UTC 2021 - Extra files


RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.symbols.bin
RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.symbols
RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.softdep
RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.devname
RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.dep.bin
RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.dep
RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.builtin.bin
RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.builtin.alias.bin
RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.alias.bin
RemoveFile /usr/lib/modules/5.13.8-arch1-1/modules.alias
RemoveFile /usr/lib/modules/5.13.8-arch1-1


# Wed Aug 11 03:00:23 PM UTC 2021 - New / changed files


CopyFile /etc/pacman.d/mirrorlist.pacnew
CopyFile /usr/lib/ghc-8.10.5/package.conf.d/package.cache
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


# Wed Aug 11 03:40:44 PM UTC 2021 - New / changed files


CreateLink /etc/systemd/system/multi-user.target.wants/cronie.service /usr/lib/systemd/system/cronie.service


# Wed Aug 11 04:32:11 PM UTC 2021 - Unknown packages


AddPackage git-crypt # Transparent file encryption in Git
AddPackage vi # The original ex/vi text editor


# Wed Aug 11 04:32:11 PM UTC 2021 - New / changed files


CopyFile /etc/systemd/system/getty@tty1.service.d/override.conf

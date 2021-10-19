# Fri Sep 17 05:16:50 PM EDT 2021 - New / changed files

CreateLink /usr/lib/jvm/default java-16-openjdk
CreateLink /usr/lib/jvm/default-runtime java-16-openjdk

# Fri Sep 17 05:16:50 PM EDT 2021 - New file properties

SetFileProperty /usr/lib/ghc-8.10.5 deleted y

# Fri Sep 17 05:17:49 PM EDT 2021 - Extra files

RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.symbols.bin
RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.symbols
RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.softdep
RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.devname
RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.dep.bin
RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.dep
RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.builtin.bin
RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.builtin.alias.bin
RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.alias.bin
RemoveFile /usr/lib/modules/5.13.9-arch1-1/modules.alias
RemoveFile /usr/lib/modules/5.13.9-arch1-1
RemoveFile /usr/lib/modules

CreateLink /etc/systemd/system/multi-user.target.wants/docker.service /usr/lib/systemd/system/docker.service

# Mon Oct 18 01:31:57 PM EDT 2021 - New file properties


# Tue Oct 19 11:19:54 AM EDT 2021 - Unknown packages

CreateLink /etc/systemd/user/sockets.target.wants/pipewire.socket /usr/lib/systemd/user/pipewire.socket


# Tue Oct 19 11:22:40 AM EDT 2021 - Unknown packages



# Tue Oct 19 11:22:40 AM EDT 2021 - Missing packages


RemovePackage cups
RemovePackage gutenprint


# Tue Oct 19 11:22:40 AM EDT 2021 - Extra files


CopyFile /etc/nsswitch.conf


# Tue Oct 19 11:22:40 AM EDT 2021 - Extra file properties


# Tue Oct 19 01:38:36 PM EDT 2021 - Unknown packages


AddPackage nmap # Utility for network discovery and security auditing
AddPackage qemu # A generic and open source machine emulator and virtualizer


# Tue Oct 19 01:38:36 PM EDT 2021 - New / changed files


CopyFile /etc/printcap
CopyFile /etc/systemd/system/cups.socket
CreateLink /etc/systemd/system/multi-user.target.wants/cups.path /usr/lib/systemd/system/cups.path
CreateLink /etc/systemd/system/printer.target.wants/cups.service /usr/lib/systemd/system/cups.service
CreateLink /etc/systemd/system/sockets.target.wants/cups.socket /etc/systemd/system/cups.socket

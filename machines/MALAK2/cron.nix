{ pkgs, ... }: {
  services.cron = {
    enable = true;
    systemCronJobs = [
      "0 * * * *  d4hines   . /etc/profile; ${pkgs.roam-backup}/bin/roam_backup.js /home/d4hines/RoamBackups > /tmp/roam_backup.log 2>&1"
    ];
  };
}

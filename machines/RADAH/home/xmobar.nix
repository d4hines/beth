let
  theme = import ../../../modules/home/theme.nix;
in ''
  Config {
    font = "Fira Code bold 14",
    bgColor = "${theme.DARK_GREY_COLOR}",
    fgColor = "${theme.PLAIN_COLOR}",
    position = BottomSize C 100 24,
    sepChar = "%", -- delineator between plugin names and straight text
    alignSep = "}{", -- separator between left-right alignment
    template = " %disku% | %cpu% | %memory%  } { <fc=${theme.PURPLE_COLOR}>OBS: %obs_scene%</fc> | DNS Hours: %dns_hours% | %date% | %time_norfolk% Norfolk | %time_paris% Paris | %time_la% Los Angeles",
    commands =
       [ Run Date "%a, %d %b %Y" "date" 10
        ,Run Cpu ["-L", "3", "-H", "50", "--normal", "green", "--high", "red"] 10
        ,Run Memory ["-t", "Mem: <usedratio>%"] 10
        ,Run DiskU [("/", "<used>/<size>")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20
        ,Run Com "cat" ["/tmp/current_obs_scene"] "obs_scene" 10
        ,Run Com "log-hours" ["stats", "dns_new", "--quiet"] "dns_hours" 10 
        ,Run DateZone "%I:%M %p" "en_US.UTF-8" "America/New_York" "time_norfolk" 10
        ,Run DateZone "%I:%M %p" "en_US.UTF-8" "Europe/Paris" "time_paris" 10
        ,Run DateZone "%I:%M %p" "en_US.UTF-8" "America/Los_Angeles" "time_la" 10
      ]
  }''

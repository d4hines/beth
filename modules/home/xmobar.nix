let
  theme = import ../../modules/home/theme.nix;
in
''
  Config {
    font = "Fira Code bold 14",
    bgColor = "${theme.DARK_GREY_COLOR}",
    fgColor = "${theme.PLAIN_COLOR}",
    position = BottomSize C 100 24,
    sepChar = "%", -- delineator between plugin names and straight text
    alignSep = "}{", -- separator between left-right alignment
    template = " %battery% | %bright% | %disku% | %cpu% | %memory% | %coretemp% } { <fc=${theme.PURPLE_COLOR}>OBS: %obs_scene%</fc> | %date% | %time_norfolk% Norfolk | %time_paris% Paris | %time_la% Los Angeles",
    commands =
       [ Run Date "%a, %d %b %Y" "date" 10
        ,Run Cpu ["-L", "3", "-H", "50", "--normal", "green", "--high", "red"] 10
        ,Run Memory ["-t", "Mem: <usedratio>%"] 10
        ,Run DiskU [("/", "<used>/<size>")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20
        ,Run CoreTemp       [ "--template" , "Temp: <core0>|<core1>|<core2>|<core3>|<core4>|<core5>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 50
        ,Run Com "cat" ["/tmp/current_obs_scene"] "obs_scene" 10
        ,Run DateZone "%I:%M %p" "en_US.UTF-8" "America/New_York" "time_norfolk" 10
        ,Run DateZone "%I:%M %p" "en_US.UTF-8" "Europe/Paris" "time_paris" 10
        ,Run DateZone "%I:%M %p" "en_US.UTF-8" "America/Los_Angeles" "time_la" 10
       ,Run BatteryP ["macsmc-ac", "macsmc-battery" ] [
        	"-t", "<acstatus>: <left>% - <timeleft>",
        	"--",
        	--"-c", "charge_full",
        	"-O", "AC",
        	"-o", "Bat",
        	"-h", "green",
        	"-l", "red"
        	] 300
         ,Run Brightness [ "-t", "\xf0eb <percent>%" ] 60
      ]
  }''

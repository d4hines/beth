{ isNixOS }:
let
  font = if isNixOS then "Fira Code" else "Fira Mono for Powerline";
  theme = import ./theme.nix;
in
''
  Config {
    font = "xft:${font}:size=14:antialias=true:hinting=true:bold,Noto Color Emoji:size=14:antialias=true:hinting=true",
    bgColor = "${theme.DARK_GREY_COLOR}",
    fgColor = "${theme.PLAIN_COLOR}",
    position = BottomSize C 100 24,
    sepChar = "%", -- delineator between plugin names and straight text
    alignSep = "}{", -- separator between left-right alignment
    template = " %cpu% | %memory% } %complice% { %date% | %time_norfolk% Norfolk | %time_paris% Paris | %time_india% Calcutta",
    commands =
       [ Run Date "%a, %d %b %Y" "date" 10
        ,Run Cpu ["-L", "3", "-H", "50", "--normal", "green", "--high", "red"] 10
        ,Run Memory ["-t", "Mem: <usedratio>%"] 10
        ,Run Com "ssh" ["arcturus.local", "curl", "--silent", "http://localhost:7000/status"] "complice" 10
        ,Run DateZone "%I:%M %p" "en_US.UTF-8" "America/New_York" "time_norfolk" 10
        ,Run DateZone "%I:%M %p" "en_US.UTF-8" "Europe/Paris" "time_paris" 10
        ,Run DateZone "%I:%M %p" "en_US.UTF-8" "Asia/Calcutta" "time_india" 10
      ]
  }''

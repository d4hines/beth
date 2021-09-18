import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import Xmobar

main :: IO ()
main = do
  xmobar config

config =
  defaultConfig
    { font = "xft:Fira Code:size=14:antialias=true:hinting=true:bold,Noto Color Emoji:size=14:antialias=true:hinting=true",
      bgColor = unsafePerformIO (getEnv "DARK_GREY_COLOR"),
      fgColor = unsafePerformIO (getEnv "PLAIN_COLOR"),
      position = BottomSize C 100 24,
      sepChar = "%", -- delineator between plugin names and straight text
      alignSep = "}{", -- separator between left-right alignment
      template = " %cpu% | %memory% } %StdinReader% { %date% | %time_norfolk% Norfolk | %time_paris% Paris | %time_india% Calcutta",
      commands =
        [ Run $ Date "%a, %d %b %Y" "date" 10
          ,Run $ Cpu ["-L", "3", "-H", "50", "--normal", "green", "--high", "red"] 10
          ,Run $ Memory ["-t", "Mem: <usedratio>%"] 10
          ,Run $ StdinReader
          ,Run $ DateZone "%I:%M %p" "en_US.UTF-8" "America/New_York" "time_norfolk" 10
          ,Run $ DateZone "%I:%M %p" "en_US.UTF-8" "Europe/Paris" "time_paris" 10
          ,Run $ DateZone "%I:%M %p" "en_US.UTF-8" "Asia/Calcutta" "time_india" 10
        ]
    }

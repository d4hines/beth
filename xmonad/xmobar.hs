import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import Xmobar

main :: IO ()
main = do
  xmobar config

config :: Config
config =
  defaultConfig
    { font = "xft:Fira Code:size=14:antialias=true:hinting=true:bold,Noto Color Emoji:size=14:antialias=true:hinting=true",
      bgColor = unsafePerformIO (getEnv "DARK_GREY_COLOR"),
      fgColor = unsafePerformIO (getEnv "PLAIN_COLOR"),
      position = BottomSize C 100 24,
      sepChar = "%", -- delineator between plugin names and straight text
      alignSep = "}{", -- separator between left-right alignment
      template = " %cpu% | %memory% } %complice% { %date% | %time_norfolk% | %time_paris%",
      commands =
        [ Run $ Date "%a, %d %b %Y" "date" 10,
          Run $ Date "%I:%M %p" "time_norfolk" 10,
          Run $ Cpu ["-L", "3", "-H", "50", "--normal", "green", "--high", "red"] 10,
          Run $ Memory ["-t", "Mem: <usedratio>%"] 10,
          -- TODO: replace with direct haskell code
          Run $ Com "paris_date" ["+%I:%M %p"] "time_paris" 10,
          -- TODO: replace with direct haskell code
          Run $ Com "complice" [] "complice" 30
        ]
    }

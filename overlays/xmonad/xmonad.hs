import System.Directory
import System.Environment (getEnv)
import System.Exit
import System.IO (hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.RotSlaves (rotAll', rotAllDown, rotSlavesDown)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers (doRectFloat, doFullFloat, isFullscreen, doFocus, isDialog)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.SpawnOnce

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal = "kitty"

myEditor = "code"

myEditorQuery = className =? "Code"

myBrowser = "chromium"

terminalQuery = title =? "scratchpad"

-- You can lookup classname using the xprop tool
-- E.g. xprop | grep "WM_CLASS"
signalQuery = className =? "Signal"

obsQuery = className =? "obs"

zoteroQuery = className =? "Zotero"

playAppQuery = className =? "PlayApp"

-- default tiling algorithm partitions the screen into two panes
myLayout = avoidStruts $ Tall nmaster delta ratio ||| Full
  where
    -- The default number of windows in the master pane
    nmaster = 1
    ratio = 2 / 3
    -- Percent of screen to increment by when resizing panes (not used)
    delta = 0 / 100

myManageHook =
  composeAll
    [ className =? "confirm" --> doFloat,
      className =? "file_progress" --> doFloat,
      className =? "dialog" --> doFloat,
      className =? "download" --> doFloat,
      className =? "error" --> doFloat,
      className =? "notification" --> doFloat,
      className =? "pinentry-gnome3" --> doFloat,
      className =? "splash" --> doFloat,
      className =? "toolbar" --> doFloat,
      className =? "supertuxkart" --> doShift "play" <+> doFocus,
      className =? "PlayApp" --> doShift "play" <+> doFocus,
      className =? "mednafen" --> doShift "play" <+> doFocus,
      isDialog --> doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7)
      -- isFullscreen --> doFullFloat
    ]
    <+> namedScratchpadManageHook myScratchpads

signal = "signal-desktop"

myScratchpads =
  [ NS "terminal" spawnTerm terminalQuery manageTerm
  ]
  where
    spawnTerm = myTerminal ++ " --title scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.8
        t = 0.1
        l = 0.1
    spawnOBS = "obs"
    manageOBS = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    manageSignal = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnZotero = "zotero"
    manageZotero = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

myKeys =
  [ ("M-S-q", io exitSuccess), -- Quits xmonad
    ("M-S-l", spawn "i3lock -i ~/lock-screen.png"), -- Quits xmonad
        -- launch dmenu
    ("M-p", spawn "rofi -show drun -theme iggy"),
    -- close focused window
    ("M-S-c", kill1),
    ("M-<Return>", windows W.swapMaster), -- Swap the focused window and the master window
    -- Rotate all the windows in the current stack and focus the master window
    ("M-S-j", rotAllDown >>= \x -> windows W.focusMaster),
    ("M-S-z", sendMessage NextLayout),
    -- Summon Scratchpads
    ("M-S-<Return>", namedScratchpadAction myScratchpads "terminal"),
    ("M-S-s", namedScratchpadAction myScratchpads "signal"),
    ("C-M-S-q", spawn "cat ~/.xmonad-shortcuts | head -n 1 | tail -1 | xargs activate-chrome-tab"),
    ("C-M-S-w", spawn "cat ~/.xmonad-shortcuts | head -n 2 | tail -1 | xargs activate-chrome-tab"),
    ("C-M-S-e", spawn "cat ~/.xmonad-shortcuts | head -n 3 | tail -1 | xargs activate-chrome-tab"),
    ("C-M-S-r", spawn "cat ~/.xmonad-shortcuts | head -n 4 | tail -1 | xargs activate-chrome-tab"),
    ("M-S-n", spawn "activate-chrome-tab https://roamresearch.com/#/app/d4hines"),
    ("M-S-1", windows $ W.shift "master"),
    ("M-S-2", windows $ W.shift "alt"),
    ("M-1", windows $ W.greedyView "master"),
    ("M-2", windows $ W.greedyView "alt"),
    ("M-3", windows $ W.greedyView "play"),
    ("C-<Space>", spawn "dunstctl close"),
    ("M-S-o", namedScratchpadAction myScratchpads "obs"),
    ("<Print>", spawn "flameshot gui"), -- IDK why this isn't working...
    ("<XF86AudioPlay>", spawn "playerctl play-pause"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute '@DEFAULT_SINK@' toggle"),
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume '@DEFAULT_SINK@' -10%"),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume '@DEFAULT_SINK@' +10%"),
    ("<F11>", spawn "amixer set Master 5%-"),
    ("<F12>", spawn "amixer set Master 5%+")
  ]

main :: IO ()
main = do
  xmonad $
    ewmh
      def
        { manageHook = myManageHook <+> manageDocks,
          handleEventHook =
            docksEventHook,
              -- fullscreenEventHook provides proper full screen behavior for
              -- e.g. Google Meet, Netflix, etc. but I've commented it out 
              -- for my gaming setup.
              -- <+> fullscreenEventHook,
          modMask = mod1Mask, -- Sets the "M" in the above key-combos to Left Alt key
          terminal = myTerminal,
          startupHook =
            do
              -- add any commands you want Xmonad to do on startup here
              spawnOnce "xrdb -merge ~/.Xresources"
              spawnOnce "spice-vdagent"
              spawnOnce "xset r rate 200 50" -- since it's not working on xinitrc for all keyboards
              spawnOnce "xmobar"
              spawnOnce myBrowser
              -- spawnOnce myEditor
              spawnOnce "yubikey-touch-detector --libnotify",
              -- spawnOnce signal,
          layoutHook = myLayout,
          logHook =
            -- A hook to make scratchpads hide when they lose focus.
            -- This is really useful, as otherwise they clog the screen.
            refocusLastLogHook
              >> nsHideOnFocusLoss myScratchpads,
          workspaces = ["master", "alt", "play"],
          keys = (\x -> mkKeymap x $ myKeys),
          focusFollowsMouse = False,
          borderWidth = 2,
          normalBorderColor =  "#282c34", -- $GREY_COLOR
          focusedBorderColor = "#56b6c2" -- $CYAN_COLOR
        }

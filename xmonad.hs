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
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
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

myBrowser = "chrome"

myBrowserQuery = className =? "Google-chrome"

terminalQuery = title =? "scratchpad"

-- You can lookup classname using the xprop tool
-- E.g. xprop | grep "WM_CLASS"
signalQuery = className =? "Signal"

obsQuery = className =? "obs"

zoteroQuery = className =? "Zotero"

-- default tiling algorithm partitions the screen into two panes
myLayout = avoidStruts $ Tall nmaster delta ratio
  where
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane.
    -- My screen is 3440x1440, and we want master pane to be
    -- 2440x(1440 - xmobar height)
    ratio = 32 / 43
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
      className =? "pinentry-gtk-2" --> doFloat,
      className =? "splash" --> doFloat,
      className =? "toolbar" --> doFloat
      -- isFullscreen --> doFullFloat
    ]
    <+> namedScratchpadManageHook myScratchpads

signal = "signal-desktop"

myScratchpads =
  [ NS "terminal" spawnTerm terminalQuery manageTerm,
    NS "obs" spawnOBS obsQuery manageOBS,
    NS "signal" signal signalQuery manageSignal,
    NS "zotero" spawnZotero zoteroQuery manageZotero
  ]
  where
    spawnTerm = myTerminal ++ " --title scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.6
        t = 0.9 - h
        l = 0.075
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
        w = 0.25
        t = 0.95 - h
        l = 1 - w
    spawnZotero = "zotero"
    manageZotero = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

myKeys =
  [ ("M-C-r", spawn "xmonad --recompile"), -- Recompiles xmonad
    ("M-S-r", spawn "xmonad --restart"), -- Restarts xmonad
    ("M-S-q", io exitSuccess), -- Quits xmonad
    -- launch dmenu
    ("M-p", spawn "dmenu_run -i -p \"Run: \" -fn 'Fira Code-14' -sb '#528bff'"), -- Dmenu
    -- close focused window
    ("M-S-c", kill1),
    -- Window navigation
    ("M-m", windows W.focusMaster), -- Move focus to the master window
    ("M-j", windows W.focusDown), -- Move focus to the next window
    ("M-k", windows W.focusUp), -- Move focus to the prev window
    ("M-<Return>", windows W.swapMaster), -- Swap the focused window and the master window
    -- Rotate all the windows in the current stack and focus the master window
    ("M-S-j", rotAllDown >>= \x -> windows W.focusMaster),
    -- Summon Scratchpads
    ("M-S-<Return>", namedScratchpadAction myScratchpads "terminal"),
    ("M-S-s", namedScratchpadAction myScratchpads "signal"),
    ("M-S-m", spawn "~/scripts/act.js https://app.slack.com/client/T59LZHQ11"),
    ("M-S-n", spawn "~/scripts/act.js https://roamresearch.com/#/app/d4hines"),
    ("M-S-z", namedScratchpadAction myScratchpads "zotero"),
    ("M-S-o", namedScratchpadAction myScratchpads "obs"),
    ("<Print>", spawn "flameshot gui"),
    ("<XF86AudioPlay>", spawn "playerctl play-pause")
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
              -- <+> fullscreenEventHook
          modMask = mod1Mask, -- Sets the "M" in the above key-combos to Left Alt key
          terminal = myTerminal,
          startupHook =
            do
              -- add any commands you want Xmonad to do on startup here
              spawnOnce "~/scripts/complice.js | xmobar"
              spawnOnce "~/scripts/browser_whitelist.js"
              spawnOnce myBrowser
              spawnOnce myEditor
              spawnOnce signal,
          layoutHook = myLayout,
          logHook =
            -- A hook to make scratchpads hide when they lose focus.
            -- This is really useful, as otherwise they clog the screen.
            refocusLastLogHook
              >> nsHideOnFocusLoss myScratchpads,
          workspaces = ["master"],
          keys = (\x -> mkKeymap x $ myKeys),
          focusFollowsMouse = False,
          borderWidth = 2,
          normalBorderColor = unsafePerformIO (getEnv "GREY_COLOR"),
          focusedBorderColor = unsafePerformIO (getEnv "CYAN_COLOR")
        }

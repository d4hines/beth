import System.Directory
import System.Environment (lookupEnv)
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
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, doFocus)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.SpawnOnce

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal = "kitty"

terminalQuery = title =? "scratchpad"

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
      className =? "splash" --> doFloat,
      className =? "toolbar" --> doFloat
      -- isFullscreen --> doFullFloat
    ]
    <+> namedScratchpadManageHook myScratchpads

myScratchpads =
  [ NS "terminal" spawnTerm terminalQuery manageTerm ]
  where
    spawnTerm = myTerminal ++ " --title scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.6
        t = 0.9 - h
        l = 0.075
myKeys =
  [ ("M-S-q", io exitSuccess), -- Quits xmonad
    ("M-p", spawn "dmenu_run -i -p \"Run: \" -fn 'Fira Code-14' -sb '#528bff'"), -- Dmenu
    -- close focused window
    ("M-S-c", kill1),
    ("<End> <End> <End>", kill1),
    ("M-<Return>", windows W.swapMaster), -- Swap the focused window and the master window
    -- Rotate all the windows in the current stack and focus the master window
    ("M-S-j", rotAllDown >>= \x -> windows W.focusMaster),
    ("M-S-z", sendMessage NextLayout),
    -- Summon Scratchpads
    ("M-S-<Return>", namedScratchpadAction myScratchpads "terminal"),
    ("C-<Space>", spawn "dunstctl close"),
    ("<Print>", spawn "flameshot gui"),
    ("<XF86AudioPlay>", spawn "playerctl play-pause"),
    ("<F11>", spawn "amixer set Master 5%-"),
    ("<F12>", spawn "amixer set Master 5%+")
  ]

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a _ = a


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
              playUrl <- liftIO $ fmap (fromMaybe "http://localhost:5173") (lookupEnv "PLAY_APP_URL")
              spawn $ "chromium --auto-open-devtools-for-tabs --user-data-dir=/tmp/chromium-display1 --kiosk " ++ playUrl,
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
          normalBorderColor =  "#282c34", -- $GREY_COLOR
          focusedBorderColor = "#56b6c2" -- $CYAN_COLOR
        }

import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown, rotAll')
import XMonad.Actions.WindowGo (runOrRaise, runOrRaiseMaster)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal = "kitty"

myEditor = "code"
myEditorQuery = className =? "Code"

myBrowser = "firefox " 
myBrowserQuery = className =? "firefox"

terminalQuery = title =? "scratchpad"
signalQuery = className =? "Signal"
notesQuery = className =? "Logseq"
zoteroQuery = className =? "Zotero"

myWorkspaces    = ["master","misc"]

-- default tiling algorithm partitions the screen into two panes
myLayout = avoidStruts $ Tall nmaster delta ratio
    where
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 3/4
     -- Percent of screen to increment by when resizing panes (not used)
     delta   = 0/100

myManageHook = composeAll
    [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "pinentry-gtk-2"  --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
     , isFullscreen -->  doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads

myScratchPads = [ NS "terminal" spawnTerm terminalQuery manageTerm
                , NS "notes" spawnNotes notesQuery manageNotes
                , NS "signal" spawnSignal signalQuery manageSignal
                , NS "zotero" spawnZotero zoteroQuery manageZotero
                ]
  where
    spawnTerm  = myTerminal ++ " --title scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.8
                 w = 0.8
                 t = 0.9 -h
                 l = 0.9 -w 
    spawnNotes  = "logseq"
    manageNotes = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w 
    spawnSignal = "signal-desktop"
    manageSignal = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.25
                 t = 0.95 -h
                 l = 1 -w 
    spawnZotero = "zotero"
    manageZotero = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w 

myKeys =
    [
    ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
    , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
    , ("M-S-q", io exitSuccess)              -- Quits xmonad
    -- launch dmenu
    , ("M-p", spawn "dmenu_run -i -p \"Run: \"") -- Dmenu
    -- close focused window
    , ("M-S-c", kill1)
    -- Window navigation
    , ("M-m", windows W.focusMaster)  -- Move focus to the master window
    , ("M-j", windows W.focusDown)    -- Move focus to the next window
    , ("M-k", windows W.focusUp)      -- Move focus to the prev window
    , ("M-<Return>", windows W.swapMaster) -- Swap the focused window and the master window
    --- Rotate all windows except master and keep focus in place 
    , ("M-C-j", rotSlavesDown )
    -- Rotate all the windows in the current stack and focus the master window
    , ("M-S-j", rotAllDown >>= \x -> windows W.focusMaster)

    -- Summon Scratchpads
    , ("M-S-<Return>", namedScratchpadAction myScratchPads "terminal")
    , ("M-S-n", namedScratchpadAction myScratchPads "notes")
    , ("M-S-s", namedScratchpadAction myScratchPads "signal")
    , ("M-S-z", namedScratchpadAction myScratchPads "zotero")
    , ("<Print>", spawn "flameshot gui")
    ]

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , handleEventHook    = docksEventHook
                               <+> fullscreenEventHook
        , modMask            = mod1Mask -- Left Alt key
        , terminal           = myTerminal
        , startupHook        =
            do
                -- dunst notification manager
                -- spawnOnce "dunst &"
                
                spawnOnce "logseq"
                spawnOnce "signal-desktop"
                spawnOnce "zotero"

                spawnOnce myBrowser
                spawnOnce myEditor
        , layoutHook         = myLayout
        , workspaces         = myWorkspaces
        , keys = (\x -> mkKeymap x $ myKeys)
        , borderWidth        = 2
        , normalBorderColor  = "#282c34"
        , focusedBorderColor = "#56b6c2" -- cyan in the theme in flake.nix
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc }
        }

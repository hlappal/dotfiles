
-- My Xmonad configuration
--
-- Heikki Lappalainen
-- heikki.lappalainen@protonmail.com

------------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------------
-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (killAllOtherCopies)
import XMonad.Actions.CycleWS -- (moveTo, shiftTo, WSType(NonEmptyWS, WSIs))
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (sinkAll)
import qualified XMonad.Actions.TreeSelect as TS
import qualified XMonad.Actions.FlexibleResize as Flex

-- Data
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks -- (avoidStruts, manageDocks, toggleStruts)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger -- (windowArrange)
import XMonad.Layout.Reflect (REFLECTX(..), REFLECTY(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import Control.Arrow (first)

-- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import Control.Applicative
import Control.Concurrent
import XMonad.Actions.FloatKeys

------------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------------
myFont :: String
myFont = "xft:Mononoki Nerd Font:regular:pixelsize=9"

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "urxvt" -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 1 -- Sets border width for windows

-- Color scheme implemented from Nord
myNormColor :: String
myNormColor = "#4c566a" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#d8dee9" -- Border color of focused windows

myHiddenColor :: String
myHiddenColor = "#8fbcbb"  -- Color of hidden workspace

myEmptyColor :: String
myEmptyColor = "#4c566a" -- Color for empty workspace

myUrgentColor :: String
myUrgentColor = "#bf616a"

myTitleColor :: String
myTitleColor = "#81a1c1"

altMask :: KeyMask
altMask = mod1Mask -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --set-zoom-fill --random /home/hlappal/Pictures/Wallpapers/ &" 
  spawnOnce "killall xcompmgr & xcompmgr -c -l0 -t0 -r0 -o.00 &"  -- Prevent shaded/dim screen share in Zoom
  spawnOnce "/usr/bin/emacs --daemon &" 
  setWMName "LG3D"

------------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig = def
  { font                = myFont
  , bgColor             = "#2e3440"
  , fgColor             = "#d8dee9"
  , bgHLight            = "#88c0d0"
  , fgHLight            = "#2e3440"
  , borderColor         = "#88c0d0"
  , promptBorderWidth   = 0
  , promptKeymap        = myXPKeymap
  , position            = Top
  , height              = 20
  , historySize         = 256
  , historyFilter       = id
  , defaultText         = []
  , showCompletionOnTab = False
  , searchPredicate     = isPrefixOf
  , alwaysHighlight     = True
  , maxComplRows        = Nothing -- set to 5 for 5 rows
  }

------------------------------------------------------------------------------
-- XPROMPT KEYMAP (emacs-like key bindings)
------------------------------------------------------------------------------
myXPKeymap :: M.Map (KeyMask, KeySym) (XP ())
myXPKeymap  = M.fromList $
  map (first $ (,) controlMask)   -- control + <key>
  [ (xK_z, killBefore)            -- kill line backwards
  , (xK_k, killAfter)             -- kill line fowards
  , (xK_a, startOfLine)           -- move to the beginning of the line
  , (xK_e, endOfLine)             -- move to the end of the line
  , (xK_m, deleteString Next)     -- delete a character foward
  , (xK_b, moveCursor Prev)       -- move cursor forward
  , (xK_f, moveCursor Next)       -- move cursor backward
  , (xK_BackSpace, killWord Prev) -- kill the previous word
  , (xK_y, pasteString)           -- paste a string
  , (xK_g, quit)                  -- quit out of prompt
  , (xK_bracketleft, quit)
  ]
  ++
  map (first $ (,) altMask)          -- meta key + <key>
  [ (xK_BackSpace, killWord Prev)    -- kill the prev word
  , (xK_f, moveWord Next)            -- move a word forward
  , (xK_b, moveWord Prev)            -- move a word backward
  , (xK_d, killWord Next)            -- kill the next word
  , (xK_n, moveHistory W.focusUp')   -- move up thru history
  , (xK_p, moveHistory W.focusDown') -- move down thru history
  ]
  ++
  map (first $ (,) 0) -- <key>
  [ (xK_Return, setSuccess True >> setDone True)
  , (xK_KP_Enter, setSuccess True >> setDone True)
  , (xK_BackSpace, deleteString Prev)
  , (xK_Delete, deleteString Next)
  , (xK_Left, moveCursor Prev)
  , (xK_Right, moveCursor Next)
  , (xK_Home, startOfLine)
  , (xK_End, endOfLine)
  , (xK_Down, moveHistory W.focusUp')
  , (xK_Up, moveHistory W.focusDown')
  , (xK_Escape, quit)
  ]

------------------------------------------------------------------------------
-- TREE SELECT
------------------------------------------------------------------------------
treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
  [ Node (TS.TSNode "programs" "most used programs" (return ()))
    [ Node (TS.TSNode "firefox" "default browser" (spawn "firefox")) []
    , Node (TS.TSNode "chrome" "play Torn" (spawn "google-chrome-stable")) []
    , Node (TS.TSNode "nemo" "file manager" (spawn "nemo")) []
    , Node (TS.TSNode "ranger" "text based file manager" (spawn "urxvt -e ranger")) []
    , Node (TS.TSNode "mailspring" "Gmail / Aalto-mail" (spawn "mailspring")) []
    , Node (TS.TSNode "electron-mail" "protonmail" (spawn "electron-mail")) []
    , Node (TS.TSNode "spotify" "music player" (spawn "spotify")) []
    , Node (TS.TSNode "discord" "open chat" (spawn "discord")) []
    , Node (TS.TSNode "slack" "open another chat" (spawn "slack")) []
    , Node (TS.TSNode "zoom" "open video chat" (spawn "zoom")) []
    ]
  , Node (TS.TSNode "power" "shutdown / reboot / suspend" (return ()))
    [ Node (TS.TSNode "shutdown" "poweroff system" (spawn "shutdown now")) []
    , Node (TS.TSNode "reboot" "reboot system" (spawn "reboot")) []
    , Node (TS.TSNode "suspend" "suspend system" (spawn "systemctl suspend")) []
    ]
  , Node (TS.TSNode "brightness" "Sets screen brightness using xbacklight" (return ()))
    [ Node (TS.TSNode "bright" "full power" (spawn "xbacklight -set 100")) []
    , Node (TS.TSNode "normal" "normal brightness (50%)" (spawn "xbacklight -set 50")) []
    , Node (TS.TSNode "dim" "quite dark" (spawn "xbacklight -set 10")) []
    , Node (TS.TSNode "very dim" "very dark" (spawn "xbacklight -set 0")) []
    ]
  , Node (TS.TSNode "xmonad" "working with xmonad" (return ()))
    [ Node (TS.TSNode "edit xmonad" "edit xmonad" (spawn ("emacsclient -c -a 'emacs' --eval '(find-file \"~/.xmonad/xmonad.hs\")'"))) []
    , Node (TS.TSNode "recompile xmonad" "recompile xmonad" (spawn "xmonad --recompile")) []
    , Node (TS.TSNode "restart xmonad" "restart xmonad" (spawn "xmonad --restart")) []
    ]
  , Node (TS.TSNode "system monitors" "system monitoring applications" (return ()))
    [ Node (TS.TSNode "htop" "a much better top" (spawn (myTerminal ++ " -e htop"))) []
    , Node (TS.TSNode "glances" "an eye on your system" (spawn (myTerminal ++ " -e glances"))) []
    ]
  ]

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd2e3440
                              , TS.ts_font         = "xft:Mononoki Nerd Font:bold:pixelsize=13"
                              , TS.ts_node         = (0xfff0f0f0, 0xff4c566a)
                              , TS.ts_nodealt      = (0xfff0f0f0, 0xff2b4252)
                              , TS.ts_highlight    = (0xff2e3440, 0xff8fbcbb)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

myTreeNavigation = M.fromList
  [ ((0, xK_Escape), TS.cancel)
  , ((0, xK_Return), TS.select)
  , ((0, xK_space),  TS.select)
  , ((0, xK_Up),     TS.movePrev)
  , ((0, xK_Down),   TS.moveNext)
  , ((0, xK_Left),   TS.moveParent)
  , ((0, xK_Right),  TS.moveChild)
  , ((0, xK_k),      TS.movePrev)
  , ((0, xK_j),      TS.moveNext)
  , ((0, xK_h),      TS.moveParent)
  , ((0, xK_l),      TS.moveChild)
  , ((0, xK_o),      TS.moveHistBack)
  , ((0, xK_i),      TS.moveHistForward)
  ]

------------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
    [ ("M-C-r", spawn "xmonad --recompile")
    , ("M-S-r", spawn "xmonad --restart")
    , ("M-S-q", io exitSuccess)

    -- Open My Preferred Terminal
    , ("M-<Return>", spawn (myTerminal))

    -- Run Prompt
    , ("M-S-<Return>", shellPrompt myXPConfig)
    
    -- Windows
    , ("M-f", sendMessage (T.Toggle "floats"))
    , ("M-<Delete>", withFocused $ windows . W.sink)
    , ("M-S-<Delete>", sinkAll)
    , ("M-S-h", sendMessage MirrorShrink)
    , ("M-S-l", sendMessage MirrorExpand)

    -- Windows navigation
    , ("M-m", windows W.focusMaster)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-<Backspace>", promote)
    , ("M1-S-<Tab>", rotSlavesDown)
    , ("M1-C-<Tab>", rotAllDown)
    , ("M-C-s", killAllOtherCopies)

    -- Tree Select
    , ("M-S-m", treeselectAction tsDefaultConfig)

    -- Layouts
    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-S-<Tab>", sendMessage FirstLayout)
    , ("M-C-M1-<Up>", sendMessage Arrange)
    , ("M-C-M1-<Down>", sendMessage DeArrange)
    , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
    , ("M-S-<Space>", sendMessage ToggleStruts)
    , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)
    , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))
    , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))

    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-C-j", sendMessage MirrorShrink)
    , ("M-C-k", sendMessage MirrorExpand)

    -- Workspaces
    , ("M-<Right>", moveTo Next NonEmptyWS)
    , ("M-<Left>", moveTo Prev NonEmptyWS)
    , ("M-S-<Right>", shiftTo Next NonEmptyWS)
    , ("M-S-<Left>", shiftTo Prev NonEmptyWS)

    -- Lpass menu
    , ("M-S-p", spawn "dmenu-lpass-nu")

    -- Emacs
    , ("M-S-e", spawn "emacsclient -c -a 'emacs'")

    -- Multimedia Keys
    , ("<XF86AudioMute>", spawn "amixer set -q Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
    , ("<XF86AudioLowerVolume>", spawn "amixer set -q Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set -q Master 5%+ unmute")
    , ("<Print>", spawn "scrot")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 2")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2")
    ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
            nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

-- Mouse bindigs
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) --Set the window to floating mode and move by dragging
  , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))                      --Raise the window to the top of the stack
  , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))                   --Set the window to floating mode and resize by dragging
  , ((modMask, button4), (\_ -> prevWS))                                                --Switch to previous workspace
  , ((modMask, button5), (\_ -> nextWS))                                                --Switch to next workspace
  , (((modMask .|. shiftMask), button4), (\_ -> shiftToPrev))                           --Send client to previous workspace
  , (((modMask .|. shiftMask), button5), (\_ -> shiftToNext))                           --Send client to next workspace
  ]

------------------------------------------------------------------------------
-- DZEN CONFIGS
------------------------------------------------------------------------------

-- Coming...
  
------------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------------
-- My workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool.

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x = [x]
    
myWorkspaces :: [String]
myWorkspaces = ["1:Web", "2:Term", "3:Dev", "4:Docs", "5:Mail", "6:Chat", "7:Torn", "8:Media", "9:Other"]
-- TODO Use special characters in workspace names
-- TODO Make workspace names clickable

------------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------------
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "firefox"        --> doShift "1:Web"
    , className =? "code-oss"       --> doShift "3:Dev"
    , className =? "Mailspring"     --> doShift "5:Mail"
    , className =? "electron-mail"  --> doShift "5:Mail"
    , className =? "Slack"          --> doShift "6:Chat"
    , className =? "discord"        --> doShift "6:Chat"
    , className =? "zoom"           --> doShift "6:Chat"
    , className =? "Google-chrome"  --> doShift "7:Torn"
    , className =? "vlc"            --> doShift "8:Media"
    , className =? "spotify"        --> doShift "8:Media" --FIXME
    , className =? "Gimp"           --> doShift "9:Other"
    , className =? "Tk"             --> doFloat
    , className =? "Toplevel"       --> doFloat
    ] -- <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------------
-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           -- $ mySpacing 3
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           -- $ mySpacing 3
           $ ResizableTall 1 (3/00) (1/2) []
oneBig   = renamed [Replace "oneBig"]
           $ limitWindows 6
           $ Mirror
           $ mkToggle (single MIRROR)
           $ mkToggle (single REFLECTX)
           $ mkToggle (single REFLECTY)
           $ OneBig (5/9) (8/12)
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20
           $ Full
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           -- $ mySpacing 3
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           -- $ mySpacing' 3
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           -- $ mySpacing' 4
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           -- $ mySpacing' 4
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = myFont
                      , activeColor         = "#292d3e"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#292d3e"
                      , inactiveBorderColor = "#292d3e"
                      , activeTextColor     = "#ffffff"
                      , inactiveTextColor   = "#d0d0d0" 
                      }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = spirals |||
                      tall |||
                      -- magnify |||
                      oneBig |||
                      -- noBorders monocle |||
                      -- floats |||
                      -- grid |||
                      noBorders tabs
                      -- threeCol |||
                      -- threeRow

------------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------------
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar /home/hlappal/.xmonad/xmobarrc"
  xmonad $ ewmh def
    { manageHook = myManageHook <+> manageDocks
    , handleEventHook = serverModeEventHookCmd <+> serverModeEventHook <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn) <+> docksEventHook
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                -- , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]"    -- Current workspace in xmobar
                , ppCurrent = xmobarColor myFocusColor "" . wrap "[" "]" -- Current workspace in xmobar
                -- , ppVisible = xmobarColor "#c3e88d" ""                   -- Visible but not current workspace
                , ppVisible = xmobarColor myNormColor ""                 -- Visible but not current workspace
                , ppHidden = xmobarColor myHiddenColor "" . wrap "" ""   -- Hidden workspaces in xmobar
                -- , ppHiddenNoWindows = xmobarColor "#F07178" ""           -- Hidden workspaces (no windows)
                , ppHiddenNoWindows = xmobarColor myEmptyColor ""        -- Hidden workspaces (no windows)
                , ppTitle = xmobarColor myTitleColor "" . shorten 80     -- Title of active window in xmobar
                , ppSep =  "<fc=#d8dee9> | </fc>"                        -- Separators in xmobar
                , ppUrgent = xmobarColor myUrgentColor "" . wrap "!" "!" -- Urgent workspace
                , ppExtras  = [windowCount]                              -- # of windows current workspace
                , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                }
    , modMask            = myModMask
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    , layoutHook         = myLayoutHook 
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , mouseBindings      = myMouseBindings
    } `additionalKeysP`    myKeys 

-- The xmonad configuration
-- Heikki Lappalainen
--
-- Thanks to Derek Taylor (DistroTube)
-- http://www.gitlab.com/dwt1/

------------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------------
-- Base
import XMonad
import XMonad.Config.Desktop
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, shiftToNext, shiftToPrev, toggleWS, WSType(NonEmptyWS, WSIs))
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Data
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks -- (avoidStruts, manageDocks, toggleStruts)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
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
import XMonad.Layout.Gaps
import XMonad.Layout.WindowArranger -- (windowArrange)
import XMonad.Layout.Reflect (REFLECTX(..), REFLECTY(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))
import Control.Arrow (first)

-- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

------------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------------
myFont :: String
myFont = "xft:Mononoki Nerd Font:regular:pixelsize=9"
-- myFont          = "xft:DejaVuSansMono Nerd Font:regular:antialias=true:pixelsize=9"

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty" -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 2 -- Sets border width for windows

myNormColor :: String
myNormColor = "#292d3e" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#bbc5ff" -- Border color of focused windows

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
  -- spawnOnce "volumeicon &"
  -- spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --aplpha 0 -- tint 0x292d3e --height 18 &"
  spawnOnce "/usr/bin/emacs --daemon &" 
  setWMName "LG3D"

------------------------------------------------------------------------------
-- GRID SELECT
------------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
  (0x31,0x2e,0x39) -- lowest inactive bg
  (0x31,0x2e,0x39) -- highest inactive bg
  (0x61,0x57,0x72) -- active bg
  (0xc0,0xa7,0x9a) -- inactive fg
  (0xff,0xff,0xff) -- active fg
                  
-- gridSelect menu layout
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
  { gs_cellheight   = 30
  , gs_cellwidth    = 200
  , gs_cellpadding  = 8
  , gs_originFractX = 0.5
  , gs_originFractY = 0.5
  , gs_font         = myFont
  }
    
spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where conf = defaultGSConfig

-- Set apps for the spawnSelected'
myAppGrid :: [(String, String)]
myAppGrid = [ ("Firefox", "firefox")
            , ("Chrome", "google-chrome-stable")
            , ("Discord", "discord")
            , ("Electron-mail", "electron-mail")
            , ("Nautilus", "nautilus")
            , ("Spotify", "spotify")
            , ("Zoom", "zoom")
            , ("Emacs", "emacs")
            , ("Slack", "slack")
            , ("Mailspring", "mailspring")
            ]

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
  --, autoComplete        = Just 100000
  , showCompletionOnTab = False
  , searchPredicate     = isPrefixOf
  , alwaysHighlight     = True
  , maxComplRows        = Nothing -- set to 5 for 5 rows
  }

myXPConfig' :: XPConfig
myXPConfig' = myXPConfig
  { autoComplete = Nothing
  }

-- A list of all the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)
             , ("p", passPrompt)
             , ("g", passGeneratePrompt)
             , ("r", passRemovePrompt)
             , ("s", sshPrompt)
             , ("x", xmonadPrompt)
             ]

-- A list of custom prompts
promptList' :: [(String, XPConfig -> String -> X (), String)]
promptList' = [ ("c", calcPrompt, "qalc") -- requires qalculate-gtk
              ]

------------------------------------------------------------------------------
-- CUSTOM PROMPTS
------------------------------------------------------------------------------
-- calcPrompt requires a cli calculator called qalcualte-gtk.
-- You could use this as a template for other custom prompts that
-- use command line programs that return a single line of output.
calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
  inputPrompt c (trim ans) ?+ \input ->
  liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim = f . f
      where f = reverse . dropWhile isSpace

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
-- SEARCH ENGINES
------------------------------------------------------------------------------
-- Xmonad has several search engines available to use located in
-- XMonad.Actions.Search. Additionally, you can add other search engines
-- such as those listed below.
archwiki, news, reddit :: S.SearchEngine

archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
news     = S.searchEngine "news" "https://news.google.com/search?q="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("d", S.duckduckgo)
             , ("g", S.google)
             , ("i", S.images)
             , ("n", news)
             , ("r", reddit)
             , ("w", S.wikipedia)
             ]

------------------------------------------------------------------------------
-- TREE SELECT
------------------------------------------------------------------------------
treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
  [ Node (TS.TSNode "power" "shutdown / reboot / suspend" (return ()))
    [ Node (TS.TSNode "shutdown" "poweroff system" (spawn "shutdown now")) []
    , Node (TS.TSNode "reboot" "reboot system" (spawn "reboot")) []
    , Node (TS.TSNode "suspend" "suspend system" (spawn "systemctl suspend")) []
    ]
  , Node (TS.TSNode "xmonad" "working with xmonad" (return ()))
    [ Node (TS.TSNode "edit xmonad" "edit xmonad" (spawn (myTerminal ++ " -e vi ~/.xmonad/xmonad.hs"))) []
    , Node (TS.TSNode "recompile xmonad" "recompile xmonad" (spawn "xmonad --recompile")) []
    , Node (TS.TSNode "restart xmonad" "restart xmonad" (spawn "xmonad --restart")) []
    ]
  , Node (TS.TSNode "brightness" "Sets screen brightness using xbacklight" (return ()))
    [ Node (TS.TSNode "bright" "full power" (spawn "xbacklight -set 100")) []
    , Node (TS.TSNode "normal" "normal brightness (50%)" (spawn "xbacklight -set 50")) []
    , Node (TS.TSNode "dim" "quite dark" (spawn "xbacklight -set 10")) []
    ]
  , Node (TS.TSNode "system monitors" "system monitoring applications" (return ()))
    [ Node (TS.TSNode "htop" "a much better top" (spawn (myTerminal ++ " -e htop"))) []
    , Node (TS.TSNode "glances" "an eye on your system" (spawn (myTerminal ++ " -e glances"))) []
    ]
  ]

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd292d3e
                              , TS.ts_font         = "xft:Mononoki Nerd Font:bold:pixelsize=13"
                              , TS.ts_node         = (0xffd0d0d0, 0xff202331)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xff292d3e)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
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
    , ("M-<Return>", spawn (myTerminal ++ " -e fish"))

    -- Run Prompt
    , ("M-S-<Return>", shellPrompt myXPConfig)
    
    -- Windows
    , ("M-f", sendMessage (T.Toggle "floats"))
    , ("M-<Delete>", withFocused $ windows . W.sink)
    , ("M-S-<Delete>", sinkAll)
    , ("M-S-h", sendMessage MirrorShrink)
    , ("M-S-l", sendMessage MirrorExpand)
    -- , ("M-S-f", withFocused $ windows . flip W.float center)
    --TODO move & resize floating windows with keyboard
    --TODO move & resize floating windows with mouse

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

    -- Grid Select
    , ("M-g g", spawnSelected' myAppGrid)
    , ("M-g t", goToSelected $ mygridConfig myColorizer)
    , ("M-g b", bringSelected $ mygridConfig myColorizer)

    -- Tree Select
    , ("M-S-t", treeselectAction tsDefaultConfig)

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

    -- Scratchpads
    , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")

    -- Lpass menu
    , ("M-S-p", spawn "dmenu-lpass-nu")

    -- Emacs
    , ("M-e e", spawn "emacsclient -c -a ''")
    , ("M-e a", spawn "emacsclient -c -a '' --eval '(org-agenda-list)'")
    , ("M-e b", spawn "emacsclient -c -a '' --eval '(ibuffer)'")
    , ("M-e d", spawn "emacsclient -c -a '' --eval '(dired nil)'")

    -- My Applications (Super+Alt+Key)
    -- e.g.:
    --, ("M-S-f", spawn (myTerminal ++ " -e fish"))

    -- Multimedia Keys
    , ("<XF86AudioMute>", spawn "amixer set -q Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
    , ("<XF86AudioLowerVolume>", spawn "amixer set -q Master 2%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set -q Master 2%+ unmute")
    , ("<Print>", spawn "scrot")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 2")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2")
    ]
    ++ [("M-s " ++ k, S.promptSearch myXPConfig' f) | (k,f) <- searchList ]
    ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
    ++ [("M-p " ++ k, f myXPConfig') | (k,f) <- promptList ]
    ++ [("M-p " ++ k, f myXPConfig' g) | (k,f,g) <- promptList' ]
        where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
              nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

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
-- myWorkspaces = clickable . map xmobarEscape
--                $ ["1:Web", "2:Term", "3:Dev", "4:Docs", "5:Mail", "6:Chat", "7:Torn", "8:Media", "9:Other"]
--   where
--     clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
--                   (i,ws) <- zip [1..9] l,
--                   let n = i ]

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
    , className =? "Spotify"        --> doShift "8:Media" --FIXME
    , className =? "Gimp"           --> doShift "9:Other"
    , className =? "Tk"             --> doFloat
    , className =? "Toplevel"       --> doFloat
    ] <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------------
-- WINDOWS
------------------------------------------------------------------------------
center = W.RationalRect (1/4) (1/4) (1/2) (1/2)

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
           $ mySpacing 3
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 3
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
           $ mySpacing 3
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ mySpacing' 3
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ mySpacing' 4
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ mySpacing' 4
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
    myDefaultLayout = tall |||
                      magnify |||
                      oneBig |||
                      noBorders monocle |||
                      floats |||
                      grid |||
                      noBorders tabs |||
                      spirals |||
                      threeCol |||
                      threeRow

------------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
               --, NS "cmus" spawnCmus findCmus manageCmus  
               ]

   where
   spawnTerm  = myTerminal ++  " -n scratchpad"
   findTerm   = resource =? "scratchpad"
   manageTerm = customFloating $ W.RationalRect l t w h
                where
                h = 0.9
                w = 0.9
                t = 0.95 -h
                l = 0.95 -w

------------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------------
main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar /home/hlappal/.xmonad/xmobarrc"
    xmonad $ ewmh def
    -- xmonad $ desktopConfig
        { manageHook = myManageHook <+> manageDocks
        , handleEventHook = serverModeEventHookCmd <+> serverModeEventHook <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn) <+> docksEventHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "" ""    -- Hidden workspaces in xmobar
                        --, ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#d0d0d0" "" . shorten 80     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook 
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = "#292d3e"
        , focusedBorderColor = "#bbc5ff"
        } `additionalKeysP`     myKeys 
        `additionalMouseBindings`
        [ ((mod4Mask, button4), (\w -> focus w >> Flex.mouseResizeWindow w))
        ]

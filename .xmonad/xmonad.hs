-- The xmonad configuration
-- Heikki Lappalainen
--
-- Thanks to Derek Taylor (DistroTube)
-- http://www.gitlab.com/dwt1/


-- IMPORTS -------------------------------------------------------------------

    -- Base
import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import Data.Maybe (isJust)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Utilities
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)  
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, wrap, pad, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat, doCenterFloat) 
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops   -- required for xcomposite in obs to work

    -- Actions
import XMonad.Actions.Minimize (minimizeWindow)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll -- (sinkAll, killAll)
import XMonad.Actions.FloatKeys
import XMonad.Actions.CycleWS -- (nextWs, prevWS, shiftToNext, shiftToPrev, toggleWS)
import XMonad.Actions.GridSelect
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.ConstrainedResize as Sqr

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace) 
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing (spacing) 
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))

    -- Prompts
import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))


-- CONFIG -------------------------------------------------------------------- 

--myFont          = "xft:Mononoki Nerd Font:regular:pixelsize=9"
myFont          = "xft:DejaVuSansMono Nerd Font:regular:antialias=true:pixelsize=9"
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "urxvt"   -- Sets default terminal
myTextEditor    = "nvim"    -- Sets default text editor
myBorderWidth   = 2         -- Sets border width for windows
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main = do
    xmproc <- spawnPipe "xmobar /home/hlappal/.xmonad/xmobarrc"
    --xmonad $ ewmh desktopConfig
    xmonad $ desktopConfig
        { manageHook = myManageHook <+> manageHook desktopConfig <+> manageDocks
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "" ""   -- Hidden workspaces in xmobar
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


-- AUTOSTART -----------------------------------------------------------------

myStartupHook = do
          spawnOnce "nitrogen --set-zoom-fill --random /home/hlappal/Pictures/Wallpapers/ &" 
          spawnOnce "killall xcompmgr & xcompmgr -c -l0 -t0 -r0 -o.00 &"  -- Prevent shaded/dim screen share in Zoom
          spawnOnce "emacs &" 
          --spawnOnce "picom &" 
          setWMName "LG3D"


-- GRID SELECT ---------------------------------------------------------------

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


-- KEYBINDINGS ---------------------------------------------------------------

myKeys =
    -- Xmonad
    [ ("C-M1-r", spawn "xmonad --recompile")
    , ("C-M-r", spawn "xmonad --restart")

    -- Grid Select
    , (("M-S-o"), spawnSelected'
        [ ("Firefox", "firefox")
        , ("Chrome", "google-chrome-stable")
        , ("Discord", "discord")
        --, ("Joplin", "joplin")
        , ("Electron-mail", "electron-mail")
        , ("PCManFM", "pcmanfm")
        , ("Spotify", "spotify")
        , ("Zoom", "zoom")
        , ("Emacs", "emacs")
        --, ("Code", "code")
        , ("Slack", "slack")
        , ("Mailspring", "mailspring")
        , ("Vifm", myTerminal ++ " -e vifm")
        ])

    -- Windows
    , ("M-S-h", sendMessage MirrorShrink)
    , ("M-S-l", sendMessage MirrorExpand)
    , ("M-S-f", withFocused $ windows . flip W.float center)
    , ("M-S-s", withFocused $ windows . W.sink)
    --TODO move & resize floating windows with keyboard
    --TODO move & resize floating windows with mouse

    -- Workspaces
    , ("M-<Right>", moveTo Next NonEmptyWS)
    , ("M-<Left>", moveTo Prev NonEmptyWS)
    , ("M-S-<Right>", shiftTo Next NonEmptyWS)
    , ("M-S-<Left>", shiftTo Prev NonEmptyWS)

    -- Scratchpads
    --, ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")

    -- Open My Preferred Terminal
    , ("M-<Return>", spawn (myTerminal))
		
    -- Dmenu Scripts (Alt+Ctr+Key)
    , ("M-S-<Return>", spawn "dmenu-recent-aliases")
    , ("M-S-p", spawn "dmenu-lpass-nu")

    -- My Applications (Super+Alt+Key)
    -- e.g.:
    --, ("M-S-f", spawn (myTerminal ++ " -e fish"))
    --, ("M-S-j", spawn (myTerminal ++ " -e joplin"))
    --, ("M-S-e", spawn ("emacs"))

    -- Multimedia Keys
    , ("<XF86AudioMute>", spawn "amixer set -q Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
    , ("<XF86AudioLowerVolume>", spawn "amixer set -q Master 2%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set -q Master 2%+ unmute")
    , ("<Print>", spawn "scrot")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
    ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
            nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))


-- WORKSPACES ----------------------------------------------------------------

--myWorkspaces :: [String]
myWorkspaces = ["1:Web", "2:Term", "3:Dev", "4:Emacs", "5:Mail", "6:Chat", "7:Torn", "8:Media", "9:Other"]

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "firefox"        --> doShift "1:Web"
    , className =? "code-oss"       --> doShift "3:Dev"
    , className =? "Emacs"          --> doShift "4:Emacs"
    , className =? "Mailspring"     --> doShift "5:Mail"
    , className =? "electron-mail"  --> doShift "5:Mail"
    , className =? "Slack"          --> doShift "6:Chat"
    , className =? "discord"        --> doShift "6:Chat"
    , className =? "zoom"           --> doShift "6:Chat"
    , className =? "Google-chrome"  --> doShift "7:Torn"
    , className =? "vlc"            --> doShift "8:Media"
    , className =? "Spotify"        --> doShift "8:Media"
    , className =? "Gimp"           --> doShift "9:Other"
    --, className =? "Tk"             --> doCenterFloat
    ] -- <+> namedScratchpadManageHook myScratchPads


-- WINDOWS -------------------------------------------------------------------

center = W.RationalRect (1/4) (1/4) (1/2) (1/2)


-- LAYOUTS -------------------------------------------------------------------

myLayoutHook = avoidStruts
                $ mouseResize
                $ windowArrange
                $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
                $ myDefaultLayout
                    where
                        myDefaultLayout = tall ||| oneBig ||| noBorders monocle

tall        = renamed [Replace "tall"]
                $ gaps [(U, 3), (R, 3), (L, 3), (D, 3)]
                $ spacing 3
                $ limitWindows 12
                $ ResizableTall 1 (3/100) (1/2) []
oneBig      = renamed [Replace "oneBig"]
                $ limitWindows 6
                $ Mirror
                $ mkToggle (single MIRROR)
                $ mkToggle (single REFLECTX)
                $ mkToggle (single REFLECTY)
                $ OneBig (5/9) (8/12)
monocle     = renamed [Replace "monocle"]
                $ limitWindows 20
                $ Full


-- SCRATCHPADS ---------------------------------------------------------------

--myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
--                --, NS "cmus" spawnCmus findCmus manageCmus  
--                ]
--
--    where
--    spawnTerm  = myTerminal ++  " -n scratchpad"
--    findTerm   = resource =? "scratchpad"
--    manageTerm = customFloating $ W.RationalRect l t w h
--                 where
--                 h = 0.9
--                 w = 0.9
--                 t = 0.95 -h
--                 l = 0.95 -w

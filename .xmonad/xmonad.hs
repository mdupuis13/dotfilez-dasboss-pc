import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
--import qualified XMonad.StackSet as W


-- Actions
import XMonad.Actions.MouseResize

-- Data
-- import Data.Char (isSpace, toUpper)
-- import Data.Maybe (fromJust)
import Data.Monoid
-- import Data.Maybe (isJust)
-- import Data.Tree
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.InsertPosition
-- import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns

-- Layout modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
--import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
-- Utils
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-- my custom local variables
myFont :: String
myFont = "xft:Sauce Code Pro Nerd Font Mono:regular:size=10:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "kitty"    -- Sets default terminal

myBrowser :: String
myBrowser = "qutebrowser "  -- Sets qutebrowser as browser

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

myNormColor :: String
myNormColor   = "#1e1d1d"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#3366ff"   -- Border color of focused windows

myStartupHook :: X ()
myStartupHook = do
    -- Turn on/off system beep.
    spawnOnce "xset b off"

    -- Set keyboard settings - 250 ms delay and 25 cps (characters per second) repeat rate.
    -- Adjust the values according to your preferances.
    spawnOnce "xset r rate 250 25"
    spawnOnce "nitrogen --restore"
    -- Compton
    spawnOnce "bl-compositor --start"
    -- Start the Conky session (the default conkyrc will run if no sessions have been set)
    spawnOnce "bl-conky-session --autostart &"
    spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --iconspacing 2 --transparent true --alpha 150 --tint 0x1e1d1d  --height 24 &"
    -- start keybinds daemon
    -- (If this clashes with some window manager's keybinds,
    -- you can move it to the window-manager-dependent section below.)
    spawnOnce "xbindkeys_autostart"
    -- Volume control for systray
    spawnOnce "pnmixer &"
    -- Start Clipboard manager
    spawnOnce "clipit &"
    -- Run the XDG autostart stuff. This requires python3-xdg to be installed.
    -- See bl-xdg-autostart --list for list of autostarted applications.
    spawnOnce "bl-xdg-autostart"
    
--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i (i * 3) i i) True (Border i i i i) True


-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ smartBorders
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ smartBorders
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
threeCol = renamed [Replace "threeCol"]
           $ smartBorders
           $ mySpacing 8
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Montserrat:SemiBold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1e1d1d"
    , swn_color             = "#3366ff"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =  withBorder myBorderWidth tall
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| grid
                                 ||| threeCol

-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 "]
myWorkspaces = ["dev", "www", "sys", "vbox", "comm", "media", "gfx"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces and the names would be very long if using clickable workspaces.
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "pinentry-gtk-2"  --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
     , title =? "Oracle VM VirtualBox Manager"  --> doFloat
     , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
     , className =? "Chromium"     --> doShift ( myWorkspaces !! 1 )
     , className =? "Brave-browser"     --> doShift ( myWorkspaces !! 1 )
     , className =? "qutebrowser"     --> doShift ( myWorkspaces !! 1 )
     , className =? "Google Hangouts – mdupuis13@gmail.com"     --> doShift ( myWorkspaces !! 4 )
     , className =? "mpv"             --> doShift ( myWorkspaces !! 5 )
     , className =? "vlc"             --> doShift ( myWorkspaces !! 5 )
     , className =? "Gimp"            --> doShift ( myWorkspaces !! 6 )
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 3 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , isFullscreen -->  doFullFloat
     ]

main :: IO ()
main = do
    -- Launching one instance of xmobar on the monitor.
    xmproc <- spawnPipe "xmobar"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def
            { manageHook         = insertPosition End Newer <+> myManageHook <+> manageDocks
            , handleEventHook    = docksEventHook
            , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#99d6ff" "" . wrap "[" "]"
                        , ppVisible = xmobarColor "#0099ff" ""               -- Visible but not current workspace
                        , ppHidden = xmobarColor "#0099ff" "" . wrap "*" ""  -- Hidden workspaces
                        , ppHiddenNoWindows = xmobarColor "#6e6d6d" ""       -- Hidden workspaces (no windows)
                        , ppLayout = xmobarColor "#99d6ff" ""
                        , ppTitle = xmobarColor "#0099ff" "" . shorten 80
                        , ppSep =   "<fc=#0099ff> | </fc>"
                        }
            , modMask = myModMask
            , terminal = myTerminal
            , startupHook = myStartupHook
            , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
            , workspaces         = myWorkspaces
            , borderWidth        = myBorderWidth
            , normalBorderColor  = myNormColor
            , focusedBorderColor = myFocusColor
         } `additionalKeys`
        [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]

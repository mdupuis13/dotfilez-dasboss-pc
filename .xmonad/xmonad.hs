import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W


-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.MouseResize
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)


-- Data
import Data.Char (isSpace, toUpper)
-- import Data.Maybe (fromJust)
import Data.Monoid
-- import Data.Maybe (isJust)
--import Data.Tree
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
--import XMonad.Hooks.ServerMode
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
--import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
-- Utils
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig(additionalKeysP)
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
myBrowser = "firefox "  -- Sets firefox as browser

myFileManager :: String
myFileManager = "thunar "  -- Sets my prefered file manager 

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

myNormColor :: String
myNormColor   = "#003366"   -- Border color of normal windows
--myNormColor   = "#1e1d1d"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#0099ff"   -- Border color of focused windows

myLightColor :: String
myLightColor  = "#99d6ff"   -- Border color of focused windows

myHiddenColor :: String
myHiddenColor  = "#6e6d6d"   -- Border color of focused windows

myNormBgColor :: String
myNormBgColor  = "#1e1d1d"

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
    -- bind special keys (double-click on mouse 9 mainly)
    spawnOnce "xbindkeys_autostart"
    -- load the tray space
    spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --iconspacing 2 --transparent true --alpha 150 --tint 0x1e1d1d  --height 24 &"
    -- Volume control for systray
    --spawnOnce "pnmixer &"
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
           $ mySpacing 5
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
           $ mySpacing 5
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
threeColMid = renamed [Replace "threeColMid"]
           $ smartBorders
           $ mySpacing 5
           $ limitWindows 7
           $ ThreeColMid 1 (3/100) (10/16)

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Montserrat:SemiBold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = myNormBgColor
    , swn_color             = myFocusColor
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =  tall
                                 ||| threeColMid
                                 ||| noBorders monocle
                                 ||| grid
                                 ||| floats

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
     , className =? "Galculator"      --> doFloat
     , className =? "Pavucontrol"      --> doFloat
     , title =? "Oracle VM VirtualBox Manager"  --> doFloat
     , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
     , className =? "Chromium"        --> doShift ( myWorkspaces !! 1 )
     , className =? "Brave-browser"   --> doShift ( myWorkspaces !! 1 )
     , className =? "qutebrowser"     --> doShift ( myWorkspaces !! 1 )
     , className =? "Google Hangouts – mdupuis13@gmail.com"     --> doShift ( myWorkspaces !! 4 )
     , className =? "mpv"             --> doShift ( myWorkspaces !! 5 )
     , className =? "Audacious"       --> doShift ( myWorkspaces !! 5 )
     , className =? "vlc"             --> doShift ( myWorkspaces !! 5 )
     , className =? "Gimp"            --> doShift ( myWorkspaces !! 6 )
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 3 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , isFullscreen -->  doFullFloat
     ]

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)              -- Quits xmonad
        , ("M-S-l", spawn "bl-lock")               -- locks session 

    -- Run Prompt
        , ("M-S-<Return>", spawn "dmenu_run -i -p \"Run: \"") -- Dmenu

    -- Other Dmenu Prompts
    -- In Xmonad and many tiling window managers, M-p is the default keybinding to
    -- launch dmenu_run, so I've decided to use M-p plus KEY for these dmenu scripts.
        , ("M-p a", spawn "dmsounds")  -- pick color from our scheme
        , ("M-p c", spawn "dmcolors")  -- pick color from our scheme
        , ("M-p e", spawn "dmconf")   -- edit config files
        , ("M-p i", spawn "dmscrot")  -- screenshots (images)
        , ("M-p k", spawn "dmkill")   -- kill processes
        , ("M-p m", spawn "dman")     -- manpages
        , ("M-p o", spawn "dmqute")   -- qutebrowser bookmarks/history
        , ("M-p p", spawn "passmenu") -- passmenu
        , ("M-p q", spawn "dmlogout") -- logout menu
        , ("M-p r", spawn "dmred")    -- reddio (a reddit viewer)
        , ("M-p s", spawn "dmsearch") -- search various search engines

    -- Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn (myTerminal))
        , ("M-b", spawn (myBrowser ++ " about:blank"))
        , ("M-M1-h", spawn (myTerminal ++ " -e htop"))
        , ("M-S-e", spawn (myFileManager))

    -- Kill windows
        , ("M-S-c", kill1)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
        , ("C-M1-j", decWindowSpacing 4)         -- Decrease window spacing
        , ("C-M1-k", incWindowSpacing 4)         -- Increase window spacing
        , ("C-M1-h", decScreenSpacing 4)         -- Decrease screen spacing
        , ("C-M1-l", incScreenSpacing 4)         -- Increase screen spacing

    -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase # of clients master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase # of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease # of windows

    -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width

    -- Set wallpaper with 'feh'. Type 'SUPER+F1' to launch sxiv in the wallpapers directory.
    -- Then in sxiv, type 'C-x w' to set the wallpaper that you choose.
    --, ("M-<F1>", spawn "sxiv -r -q -t -o ~/wallpapers/*")
    --, ("M-<F2>", spawn "/bin/ls ~/wallpapers | shuf -n 1 | xargs xwallpaper --stretch")
        --, ("M-<F2>", spawn "feh --randomize --bg-fill ~/wallpapers/*")

    -- Controls for mocp music player (SUPER-u followed by a key)
        , ("M-u p", spawn "mocp --play")
        , ("M-u l", spawn "mocp --next")
        , ("M-u h", spawn "mocp --previous")
        , ("M-u <Space>", spawn "mocp --toggle-pause")

    -- Multimedia Keys
        , ("M-v", spawn "pavucontrol")
        , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
        , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
        , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
        , ("<XF86AudioStop>", spawn "amixer set Master toggle")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86Explorer>", spawn myFileManager)
        , ("<XF86HomePage>", spawn (myBrowser ++ " about:blank"))
        , ("<XF86Mail>", runOrRaise "claws-mail" (resource =? "claws-mail"))
        , ("<XF86Calculator>", runOrRaise "galculator" (resource =? "galculator"))
        , ("<XF86Tools>", runOrRaise "audacious" (resource =? "audacious"))
        , ("<Print>", spawn "dmscrot")
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
                        , ppCurrent = xmobarColor myLightColor "" . wrap "[" "]"
                        , ppVisible = xmobarColor myFocusColor ""               -- Visible but not current workspace
                        , ppHidden = xmobarColor myFocusColor "" . wrap "*" ""  -- Hidden workspaces
                        , ppHiddenNoWindows = xmobarColor myHiddenColor ""       -- Hidden workspaces (no windows)
                        , ppLayout = xmobarColor myLightColor ""
                        , ppTitle = xmobarColor myFocusColor "" . shorten 80
                        , ppSep =   "<fc=#3366ff> | </fc>"
                        }
            , modMask = myModMask
            , terminal = myTerminal
            , startupHook = myStartupHook
            , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
            , workspaces         = myWorkspaces
            , borderWidth        = myBorderWidth
            , normalBorderColor  = myNormColor
            , focusedBorderColor = myFocusColor
         } `additionalKeysP` myKeys


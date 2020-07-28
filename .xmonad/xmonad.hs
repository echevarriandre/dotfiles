-- Heavily based on https://gitlab.com/dwt1/dotfiles/-/tree/master/.xmonad

import XMonad

import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.UpdatePointer

import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (docks, avoidStruts, manageDocks, ToggleStruts(..))

import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.ResizableTile

import Data.Monoid
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

myFont :: String
myFont = "xft:mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"   -- Set`s default terminal

myBrowser :: String
myBrowser = "firefox"               -- Sets firefox as browser for tree select
-- myBrowser = myTerminal ++ " -e lynx " -- Sets lynx as browser for tree select

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColor :: String
myNormColor   = "#000"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#bbc5ff"  -- Border color of focused windows

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myStartupHook :: X ()
myStartupHook = do
        spawnOnce "nitrogen --restore &"            -- restore wallpaper
        spawnOnce "autorandr -c &"                  -- auto set monitor order based on saved configuration
        spawnOnce "xsetroot -cursor_name left_ptr"  -- remove X cursor https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Setting_the_X_cursor
        spawnOnce "setxkbmap -option compose:ralt"  -- set compose key to write accented characters
        spawnOnce "xinput --set-prop 9 'libinput Accel Speed' -0.65"  -- set mouse speed
        spawnOnce "picom -CG &"
        spawnOnce "redshift &"

myManageHook = composeAll
     [ 
        className =? "KeePassXC" --> doFloat,
        className =? "discord" --> doFloat,
        title =? "Nitrogen" --> doFloat
     ] <+> manageDocks

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Define gaps and limit windows
tall     = renamed [Replace "tall"]
            $ limitWindows 6
            $ mySpacing 8
            $ ResizableTall 1 (3/100) (1/2) []

myLayoutHook = myDefaultLayout
                where
                    myDefaultLayout = tall

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount >> updatePointer (0.5, 0.5) (0, 0)
    where fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys =
    [
    -- Xmonad
        ("M-C-r", spawn "xmonad --recompile"),      -- Recompiles xmonad
        ("M-S-r", spawn "xmonad --restart"),        -- Restarts xmonad
        ("M-S-M1-q", io exitSuccess),                  -- Quits xmonad
        ("M-<Return>", spawn myTerminal),

    -- Windows
        ("M-S-c", kill1),                           -- Kill the currently focused client
        ("M-S-a", killAll),                         -- Kill all windows on current workspace

    -- Windows Navigation
        ("M-m", windows W.focusMaster),            -- Move focus to the master window
        ("M-k", windows W.focusDown),              -- Move focus to the next window
        ("M-j", windows W.focusUp),                -- Move focus to the prev window
        ("M-S-m", windows W.swapMaster),           -- Swap the focused window and the master window
        ("M-S-k", windows W.swapDown),             -- Swap focused window with next window
        ("M-S-j", windows W.swapUp),               -- Swap focused window with prev window
        ("M-<Backspace>", promote),                -- Moves focused window to master, others maintain order

    -- Workspaces
        ("M-.", nextScreen),                       -- Switch focus to next monitor
        ("M-,", prevScreen),                       -- Switch focus to prev monitor
    
    -- Multimedia Keys
        --, ("<XF86AudioPlay>", spawn "cmus toggle")
        --, ("<XF86AudioPrev>", spawn "cmus prev")
        --, ("<XF86AudioNext>", spawn "cmus next")
        ("<XF86AudioMute>",   spawn "amixer set Master toggle"),
        ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
        ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute"),
        ("<Print>", spawn "scrot -u ~/Pictures/screenshots/$(date +%F_%T).png -e 'xclip -selection c -t image/png < $f'"),
        ("C-<Print>", spawn "sleep 0.2; scrot -s -f ~/Pictures/screenshots/$(date +%F_%T).png -e 'xclip -selection c -t image/png < $f'"),

    -- Applications
        ("M-M1-f", spawn "firefox"),
        ("M-M1-e", spawn "thunar"),
        ("M-<Space>", spawn "rofi -show run"),
        ("M-C-<Space>", spawn "rofi -show"),
        ("M-M1-c", spawn "code"),
        ("M-M1-a", spawn "keepassxc"),
        ("M-M1-d", spawn "discord"),

    -- System
        ("M-S-<Escape>", spawn "reboot"),
        ("M-<Escape>", spawn "shutdown now"),

    -- Layouts
        ("M-S-<Tab>", rotSlavesDown),              -- Rotate all windows except master and keep focus in place
        ("M-C-<Tab>", rotAllDown),                 -- Rotate all the windows in the current stack
        ("M-<KP_Multiply>", sendMessage (IncMasterN 1)),   -- Increase number of clients in master pane
        ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in master pane

    ]

myDeletedKeys :: [(String)]
myDeletedKeys = 
    [ ("M-p")       -- Remove default open menu
    -- , ("M-S-q")     -- Remove default quit xmonad
    ]

main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 /home/oxy/.config/xmobar/xmobarrc0"
    xmproc1 <- spawnPipe "xmobar -x 0 /home/oxy/.config/xmobar/xmobarrc1"
    xmonad $ docks $ ewmh def
        {
            layoutHook            = avoidStruts $ myLayoutHook,
            manageHook            = myManageHook <+> manageDocks,
            modMask               = myModMask,
            terminal              = myTerminal,
            startupHook           = myStartupHook,
            workspaces            = myWorkspaces,
            borderWidth           = myBorderWidth,
            normalBorderColor     = myNormColor,
            focusedBorderColor    = myFocusColor,
            logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                            { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
                            , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                            , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                            , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                            , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                            , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                            , ppSep =  "<fc=#666666> <fn=2></fn> </fc>"                     -- Separators in xmobar
                            , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                            , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                            }

        } `additionalKeysP` myKeys `removeKeysP` myDeletedKeys

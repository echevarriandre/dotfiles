-- Heavily based on https://gitlab.com/dwt1/dotfiles/-/tree/master/.xmonad

import XMonad

import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.WithAll (sinkAll, killAll)

import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.SpawnOnce

import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.ResizableTile

import Data.Monoid
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)

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
     [ className =? "KeePassXC" --> doFloat
     , className =? "discord" --> doFloat
     ]

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad
        , ("M-<Return>", spawn myTerminal)

    -- Windows
        , ("M-S-c", kill1)                           -- Kill the currently focused client
        , ("M-S-a", killAll)                         -- Kill all windows on current workspace

    -- Windows Navigation
        , ("M-m", windows W.focusMaster)            -- Move focus to the master window
        , ("M-k", windows W.focusDown)              -- Move focus to the next window
        , ("M-j", windows W.focusUp)                -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster)           -- Swap the focused window and the master window
        , ("M-S-k", windows W.swapDown)             -- Swap focused window with next window
        , ("M-S-j", windows W.swapUp)               -- Swap focused window with prev window
        , ("M-<Backspace>", promote)                -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack

    -- Workspaces
        , ("M-.", nextScreen)                       -- Switch focus to next monitor
        , ("M-,", prevScreen)                       -- Switch focus to prev monitor
    
    -- Multimedia Keys
        --, ("<XF86AudioPlay>", spawn "cmus toggle")
        --, ("<XF86AudioPrev>", spawn "cmus prev")
        --, ("<XF86AudioNext>", spawn "cmus next")
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<Print>", spawn "scrot -u ~/Pictures/screenshots/$(date +%F_%T).png -e 'xclip -selection c -t image/png < $f'")
        , ("C-<Print>", spawn "sleep 0.2; scrot -s -f ~/Pictures/screenshots/$(date +%F_%T).png -e 'xclip -selection c -t image/png < $f'")

    -- Applications
        , ("M-f", spawn "firefox")
        , ("M-e", spawn "thunar")
        , ("M-<Space>", spawn "rofi -show run")
        , ("M-C-<Space>", spawn "rofi -show")
        , ("M-c", spawn "code")
        , ("M-a", spawn "keepassxc")
        , ("M-d", spawn "discord")

        , ("M-S-<Escape>", spawn "reboot")
        , ("M-<Escape>", spawn "shutdown now")
    ]

myDeletedKeys :: [(String)]
myDeletedKeys = 
    [ ("M-p")       -- Remove default open menu
    , ("M-S-q")     -- Remove default quit xmonad
    ]

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

main :: IO ()
main = do
    xmonad $ ewmh def
        {
            manageHook =  myManageHook
            , modMask               = myModMask
            , terminal              = myTerminal
            , layoutHook            = myLayoutHook
            , startupHook           = myStartupHook
            , workspaces            = myWorkspaces
            , borderWidth           = myBorderWidth
            , normalBorderColor     = myNormColor
            , focusedBorderColor    = myFocusColor
        } `additionalKeysP` myKeys `removeKeysP` myDeletedKeys

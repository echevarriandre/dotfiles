-- Heavily based on https://gitlab.com/dwt1/dotfiles/-/tree/master/.xmonad

import XMonad

import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.NoBorders

import XMonad.Util.Cursor as Cur
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP, additionalMouseBindings)
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.NamedScratchpad

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicBars
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
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))

import Data.Monoid
import Data.Map (Map(), fromList)
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

myFont :: String
myFont = "xft:mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

-- Sets modkey to super/windows key
myModMask :: KeyMask
myModMask = mod4Mask

-- Sets default terminal
myTerminal :: String
myTerminal = "alacritty"

-- Sets border width for windows
myBorderWidth :: Dimension
myBorderWidth = 2

-- Border color of normal windows
myNormColor :: String
myNormColor   = "#000"

 -- Border color of focused windows
myFocusColor :: String
myFocusColor  = "#ff79c6"

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myStartupHook :: X ()
myStartupHook = do
        setDefaultCursor xC_left_ptr                                    -- fix default cursor being a cross
        -- spawnOnce "xsetroot -cursor_name left_ptr"                      -- remove X cursor https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Setting_the_X_cursor
        spawnOnce "nitrogen --restore &"                                -- restore wallpaper
        spawnOnce "autorandr -c &"                                      -- auto set monitor order based on saved configuration
        spawnOnce "setxkbmap -option compose:ralt"                      -- set compose key to write accented characters
        spawnOnce "xinput --set-prop 9 'libinput Accel Speed' -0.65"    -- set mouse speed
        spawnOnce "picom -CG &"
        spawnOnce "redshift &"

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ 
        -- Use xprops to find class name
        className =? "KeePassXC" --> doFloat,
        -- className =? "discord" --> doFloat,
        className =? "Gnome-calculator" --> doFloat,
        className =? "Nitrogen" --> doFloat
    ] <+> namedScratchpadManageHook myScratchPads <+> manageDocks

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Define gaps and limit windows
tall    = renamed [Replace "tall"]
        $ limitWindows 6
        $ mySpacing 8
        $ ResizableTall 1 (3/100) (1/2) []

myLayoutHook = tall

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount >> updatePointer (0.5, 0.5) (0, 0)
    where fadeAmount = 1.0

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                ]
  where
    spawnTerm  = myTerminal ++ " --class scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

barCreator :: DynamicStatusBar
barCreator (S sid) = spawnPipe $ "xmobar --screen " ++ show sid ++ " /home/oxy/.config/xmobar/xmobarrc.hs"

barDestroyer :: DynamicStatusBarCleanup
barDestroyer = return ()

myLogPP :: PP
myLogPP = xmobarPP {
    ppCurrent = xmobarColor "#a7c3ff" "" . wrap "[" "]",
    ppVisible = xmobarColor "#82AAFF" "". wrap "*" "",
    ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "",
    ppTitle = xmobarColor "#b3afc2" "" . shorten 60,
    ppHiddenNoWindows = \str -> "",
    ppLayout = \str -> "",
    ppSep =  "  ",
    ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
}

myLogPPActive :: PP
myLogPPActive = myLogPP {
    ppCurrent = xmobarColor "#ffff00" "" . wrap "[" "]"
}

main :: IO ()
main = do
    xmonad $ docks def
        {
            layoutHook              = avoidStruts $ myLayoutHook,
            manageHook              = myManageHook <+> manageDocks,
            modMask                 = myModMask,
            terminal                = myTerminal,
            startupHook             = myStartupHook <+> dynStatusBarStartup barCreator barDestroyer,
            workspaces              = myWorkspaces,
            borderWidth             = myBorderWidth,
            normalBorderColor       = myNormColor,
            focusedBorderColor      = myFocusColor,
            logHook                 = workspaceHistoryHook <+> myLogHook <+> multiPP myLogPPActive myLogPP,
            handleEventHook         = dynStatusBarEventHook barCreator barDestroyer
        } `additionalKeysP` myKeys `removeKeysP` myDeletedKeys `additionalMouseBindings` myMouseKeys


--------------------
--  KEY BINDINGS  --
--------------------
myKeys :: [(String, X ())]
myKeys =
    [
    -- Xmonad
        ("M-C-r", spawn "xmonad --recompile"),      -- Recompiles xmonad
        ("M-S-r", spawn "xmonad --restart"),        -- Restarts xmonad
        ("M-S-M1-q", io exitSuccess),               -- Quits xmonad
        ("M-<Return>", spawn myTerminal),

    -- Windows
        ("M-S-c", kill1),                           -- Kill the currently focused client
        ("M-S-a", killAll),                         -- Kill all windows on current workspace

    -- Windows Navigation
        ("M-m", windows W.focusMaster),            -- Move focus to the master window
        ("M-k", windows W.focusDown),              -- Move focus to the next window
        ("M-j", windows W.focusUp),                -- Move focus to the prev window
        -- ("M-S-m", windows W.swapMaster),           -- Swap the focused window and the master window
        ("M-<Backspace>", promote),                -- Moves focused window to master, others maintain order
        ("M-S-k", windows W.swapDown),             -- Swap focused window with next window
        ("M-S-j", windows W.swapUp),               -- Swap focused window with prev window

    -- Workspaces
        ("M-,", prevScreen),                       -- Switch focus to prev monitor
        ("M-.", nextScreen),                       -- Switch focus to next monitor

        -- Scratchpads
        ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal"),

    
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
        ("M-S-<Tab>", rotSlavesDown),                       -- Rotate all windows except master and keep focus in place
        ("M-C-<Tab>", rotAllDown),                          -- Rotate all the windows in the current stack
        ("M-<KP_Multiply>", sendMessage (IncMasterN 1)),    -- Increase number of clients in master pane
        ("M-<KP_Divide>", sendMessage (IncMasterN (-1))),    -- Decrease number of clients in master pane
        ("M-h", sendMessage Shrink),                       -- Shrink horiz window width
        ("M-l", sendMessage Expand),                       -- Expand horiz window width
        ("M-C-j", sendMessage MirrorShrink),               -- Shrink vert window width
        ("M-C-k", sendMessage MirrorExpand),               -- Exoand vert window width
        -- ("M-<F11>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
        ("M-<F11>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts <+> withFocused toggleBorder)
    ] ++
    [
        -- Change screen order to use with SUPER+[w, e]
        (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | (key, scr)  <- zip "we" [1,0] -- was [0,1,2]
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]

myMouseKeys = [
    ((myModMask, button4), const $ sendMessage Expand),
    ((myModMask, button5), const $ sendMessage Shrink),
    ((myModMask .|. shiftMask, button4), const $ sendMessage MirrorExpand),
    ((myModMask .|. shiftMask, button5), const $ sendMessage MirrorShrink)
    ]

myDeletedKeys :: [(String)]
myDeletedKeys = 
    [ ("M-p")        -- Remove default open menu
    , ("M-S-q")      -- Remove default quit xmonad
    , ("M-q")        -- Remove default restart xmonad
    ]

-- Heavily based on https://gitlab.com/dwt1/dotfiles/-/tree/master/.xmonad

import XMonad

import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.NoBorders
import XMonad.Actions.DynamicProjects
import XMonad.Actions.SpawnOn
import qualified XMonad.Actions.TreeSelect as TS

import XMonad.Util.Cursor as Cur
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP, additionalMouseBindings)
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe, runInTerm)
import XMonad.Util.NamedScratchpad

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (docks, avoidStruts, manageDocks, ToggleStruts(..))

import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.ResizableTile
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))

import Data.Tree
import Data.Monoid
import Data.Map (Map(), fromList)
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

-----------------
--  VARIABLES  --
-----------------
myFont = "xft:mononoki Nerd Font:bold:size=10:antialias=true:hinting=true"
myModMask = mod4Mask
myBorderWidth = 2
myNormColor   = "#000"
myFocusColor  = "#ff79c6"

---------------------------------------------------
--  THEME (https://draculatheme.com/contribute/) --
---------------------------------------------------
backgroundColor = "#282a36"
focusColor      = "#44475a"
textColor       = "#f8f8f2"
cyan            = "#8be9fd"
green           = "#50fa7b"
orange          = "#ffb86c"
pink            = "#ff79c6"
purple          = "#bd93f9"
red             = "#ff5555"
yellow          = "#f1fa8c"
black           = "#000000"
white           = "#ffffff"

--------------------
--  APPLICATIONS  --
--------------------

myTerminal = "alacritty"
myBrowser = "firefox"
myEditor = myTerminal ++ " -e vim "
myLauncher = "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun"

--------------------------------------
--  WORKSPACES CLICKABLE IN XMOBAR  --
--------------------------------------
ws1 = "www"
ws2 = "dev"
ws3 = "sys"
ws4 = "4"
ws5 = "5"
ws6 = "6"
ws7 = "7"
ws8 = "8"
ws9 = "9"

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9]

-- Clickable workspaces, doesn't work with dynamic projects
-- myWorkspaces = clickable . (map xmobarEscape)
--             $ [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9]
--         where
--         clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
--                       (i,ws) <- zip [1..9] l,
--                       let n = i ]

-- DynamicProjects
projects :: [Project]
projects =  [ Project   { projectName       = ws3
                        , projectDirectory  = "~/"
                        , projectStartHook  = Just $ do spawnOn ws3 myTerminal
                                                        spawnOn ws3 myTerminal
                                                        spawnOn ws3 myTerminal
                        }
            , Project   { projectName       = ws1
                        , projectDirectory  = "~/"
                        , projectStartHook  = Just $ do spawnOn ws1 myBrowser
                        }
            , Project   { projectName       = ws2
                        , projectDirectory  = "~/"
                        , projectStartHook  = Just $ do spawnOn ws2 "code"
                        }
            , Project   { projectName       = ws9
                        , projectDirectory  = "~/"
                        , projectStartHook  = Just $ do spawnOn ws9 "firefox --private-window" 
                        }
            ]

------------------
--  TREESELECT  --
------------------

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "+ code" "" (return ()))
        [ Node (TS.TSNode "vs code" "" (spawn "code")) []
        , Node (TS.TSNode "insomnia" "" (spawn "insomnia")) []
        , Node (TS.TSNode "tableplus" "" (spawn "tableplus")) []
        ]
    , Node (TS.TSNode "+ apps" "" (return ()))
        [ Node (TS.TSNode "firefox" "" (spawn "firefox")) []
        , Node (TS.TSNode "discord" "" (spawn "discord")) []
        , Node (TS.TSNode "redshift" "" (spawn "redshift")) []
        , Node (TS.TSNode "qutebrowser" "" (spawn "qutebrowser")) []
        , Node (TS.TSNode "calculator" "" (spawn "gnome-calculator")) []
        , Node (TS.TSNode "thunar" "" (spawn "thunar")) []
        , Node (TS.TSNode "virtualbox" "" (spawn "VirtualBox")) []
        , Node (TS.TSNode "vlc" "" (spawn "vlc")) []
        ]
    , Node (TS.TSNode "+ system" "" (return ()))
        [ Node (TS.TSNode "alacritty" "" (spawn "alacritty")) []
        , Node (TS.TSNode "lxappearance" "" (spawn "lxappearance")) []
        , Node (TS.TSNode "pavucontrol" "" (spawn "pavucontrol")) []
        , Node (TS.TSNode "nitrogen" "" (spawn "nitrogen")) []
        , Node (TS.TSNode "nvidia settings" "" (spawn "nvidia-settings")) []
        , Node (TS.TSNode "htop" "" (spawn (myTerminal ++ " -e htop"))) []
        ]
    , Node (TS.TSNode "+ xmonad" "" (return ()))
        [ Node (TS.TSNode "Recompile" "" (spawn "xmonad --recompile")) []
        , Node (TS.TSNode "Restart" "" (spawn "xmonad --restart")) []
        , Node (TS.TSNode "Next layout" "" (spawn "~/.xmonad/xmonadctl next-layout")) []
        , Node (TS.TSNode "Quit" "" (io exitSuccess)) []
        ]
    , Node (TS.TSNode "+ power" "" (return ()))
        [ Node (TS.TSNode "reboot" "" (spawn "reboot")) []
        , Node (TS.TSNode "shutdown" "" (spawn "shutdown now")) []
        ]
   ]


tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0x00000000
                              , TS.ts_font         = myFont
                              , TS.ts_node         = (0xffffffff, 0xff000000)
                              , TS.ts_nodealt      = (0xffffffff, 0xff000000)
                              , TS.ts_highlight    = (0xffffffff, 0xffff79c6)
                              , TS.ts_extra        = 0xffffff00
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 23
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 20
                              , TS.ts_navigate     = myTreeNavigation
                              }



myTreeNavigation = fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    ]

----------------------
--  CONFIGURATIONS  --
------------------------

myStartupHook :: X ()
myStartupHook = do
        setDefaultCursor xC_left_ptr                                    -- fix default cursor being a cross
        -- spawnOnce "xsetroot -cursor_name left_ptr"                      -- remove X cursor https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Setting_the_X_cursor
        spawnOnce "nitrogen --restore &"                                -- restore wallpaper
        spawnOnce "autorandr -c &"                                      -- auto set monitor order based on saved configuration
        spawnOnce "setxkbmap -option compose:ralt"                      -- set compose key to write accented characters
        spawnOnce "xinput --set-prop $(xinput list | grep -w \"Logitech G403 Prodigy Gaming Mouse\" | head -n 1 | awk '{print $8}' | cut -d'=' -f2) 'libinput Accel Speed' -0.65"    -- set mouse speed
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

-- Change first boolean for smart borders
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Define gaps and limit windows
tall    = renamed [Replace "tall"]
        $ limitWindows 6
        $ mySpacing 4
        $ ResizableTall 1 (3/100) (5/8) []
tabs    = renamed [Replace "tabs"]
        $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=11"
                      , activeColor         = focusColor
                      , inactiveColor       = backgroundColor
                      , activeBorderColor   = focusColor
                      , inactiveBorderColor = backgroundColor
                      , activeTextColor     = white
                      , inactiveTextColor   = textColor
                      }

myLayoutHook = tall ||| tabs

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount >> updatePointer (0.5, 0.5) (0, 0)
    where fadeAmount = 1.0

-------------------
--  SCRATCHPADS  --
-------------------

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

--------------
--  XMOBAR  --
--------------

barCreator :: DynamicStatusBar
barCreator (S sid) = spawnPipe $ "xmobar --screen " ++ show sid ++ " /home/oxy/.config/xmobar/xmobarrc.hs"

barDestroyer :: DynamicStatusBarCleanup
barDestroyer = return ()

myLogPP :: PP
myLogPP = xmobarPP {
    ppCurrent = xmobarColor pink "" . wrap "(" ")",
    ppVisible = xmobarColor focusColor "",
    ppHidden = xmobarColor focusColor "",
    ppTitle = xmobarColor textColor "" . shorten 60,
    ppHiddenNoWindows = \str -> "",
    ppLayout = \str -> "",
    ppSep =  "  ",
    ppUrgent = xmobarColor red "" . wrap "!" "!"
}

myLogPPActive :: PP
myLogPPActive = myLogPP {
    ppCurrent = xmobarColor yellow "" . wrap "(" ")"
}

------------
--  MAIN  --
------------

main :: IO ()
main = do
    xmonad 
        $ docks
        $ dynamicProjects projects
        def {
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
        ("M-<Space>", spawn myLauncher),
        -- ("M-C-<Space>", spawn "rofi -show"),
        ("M-M1-c", spawn "code"),
        ("M-M1-a", spawn "keepassxc"),
        ("M-M1-d", spawn "discord"),

    -- Tree Select/
        ("M-t", treeselectAction tsDefaultConfig),


    -- System
        ("M-S-<Escape>", spawn "reboot"),
        ("M-<Escape>", spawn "shutdown now"),

    -- Layouts
        ("M-C-l", sendMessage NextLayout),                -- Change to next layout
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
        -- Not needed if nvidia driver is installed
        -- (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
        --  | (key, scr)  <- zip "we" [1,0] -- was [0,1,2]
        --  , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
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

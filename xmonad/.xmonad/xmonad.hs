-- xmonad example config file.

import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Non-default layouts from contrib
import XMonad.Layout.HintedGrid
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns

-- For the multimedia keys
import XMonad.Util.EZConfig

-- Puts screens in a left-to-right order as reported by Xinerama
import XMonad.Actions.PhysicalScreens

-- For command line arguments
import System.Environment

-- For spawning stuff on specific workspaces
import XMonad.Actions.SpawnOn

-- For replicateM_ (prevLayout)
import Control.Monad

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal    = "sakura"
myBrowser     = "google-chrome-stable"
myLauncher    = "dmenu_run"
myFileManager = "pcmanfm"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
myBorderWidth   = 2

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#aaaaaa"
myFocusedBorderColor = "#ff0000"

-- modMask lets you specify which modkey you want to use. The default
-- mod1Mask is "left alt", mod3Mask is "right alt" and mod4Mask is "super".
myModMask       = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = map show [1..9]


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch a program launcher
    , ((modm,               xK_p     ), spawn myLauncher)

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    -- Rotate backwards through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), do { replicateM_ (noOfLayouts-1) $ sendMessage NextLayout})

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_d ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    --, ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask  , xK_i     ), spawn "xmonad --recompile; xmonad --restart")

    , ((mod4Mask, xK_t), spawn myTerminal)
    , ((mod4Mask, xK_z), spawn myBrowser)
    , ((mod4Mask, xK_x), spawn myFileManager)
    , ((mod4Mask, xK_c), kill)

    ] ++ concat [musicKeys, wsSwitch, scrSwitch]
    where
        cmusCmd :: MonadIO m => KeySym -> String -> ((KeyMask, KeySym), m ())
        cmusCmd k c = ((mod4Mask, k), spawn $ "cmus-remote -C '" ++ c ++ "'")

        cmusPairs = [ (xK_KP_Home     , "seek -60")
                    , (xK_KP_Page_Up  , "seek +60")
                    , (xK_KP_Left     , "seek -30")
                    , (xK_KP_Begin    , "player-pause")
                    , (xK_KP_Right    , "seek +30")
                    , (xK_KP_End      , "seek -5")
                    , (xK_KP_Page_Down, "seek +5")
                    -- hjkl
                    , (xK_y           , "seek -60")
                    , (xK_o           , "seek +60")
                    , (xK_h           , "seek -30")
                    , (xK_l           , "seek +30")
                    , (xK_n           , "seek -5")
                    , (xK_period      , "seek +5")
                    , (xK_u           , "player-prev")
                    , (xK_i           , "player-next")
                    , (xK_m           , "vol -5%")
                    , (xK_comma       , "vol +5%")
                    , (xK_j           , "player-pause")
                    ]

        musicKeys = map (uncurry cmusCmd) cmusPairs

        -- mod-shift-[1..9], Move client to workspace N
        wsSwitch = [((m .|. modm, k), windows $ f i)
                   | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
                   , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

        -- mod-{q,w,e}, Switch to actual screens 1, 2, or 3
        -- mod-shift-{q,w,e}, Move client to screen 1, 2, or 3
        -- requires XMonad.Actions.PhysicalScreens
        scrSwitch = [((modm .|. mask, key), f sc)
                    | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
                    , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--

myLayout       = fst countedLayouts
noOfLayouts    = snd countedLayouts
countedLayouts = (reflectHoriz tiled, 1)
              |+| tiled
              |+| Mirror tiled
              |+| noBorders Full
              |+| Grid False
              |+| GridRatio 1 True
              |+| ThreeColMid nmaster delta (1/3)
  where
    (a,n) |+| b = (a ||| b, n+1)

    nmaster  = 1     -- The default number of windows in the master pane
    colRatio = 2/3   -- Default proportion of screen occupied by master pane
    delta    = 5/100 -- Percent of screen to increment by when resizing panes

    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta colRatio


------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    args <- getArgs
    case length args of
        0 -> xmonad (defaults { startupHook = sh })
        _ -> xmonad defaults
    where
        sh = do
            spawnOn "2" myTerminal
            spawnOn "2" myTerminal
            spawnOn "2" myBrowser

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = manageSpawn <+> myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
    `additionalKeysP`
    [ ("<XF86AudioLowerVolume>", spawn "amixer -c 0 set Master 3-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 set Master 3+ unmute")
    , ("<XF86AudioMute>", spawn "amixer sset Master toggle")
    , ("<XF86AudioPlay>", spawn "cmus-remote -u")
    , ("<XF86AudioStop>", spawn "cmus-remote -s")
    , ("<XF86AudioPrev>", spawn "cmus-remote -r")
    , ("<XF86AudioNext>", spawn "cmus-remote -n")
    ]

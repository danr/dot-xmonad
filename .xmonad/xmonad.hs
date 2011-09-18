-- XMonad configuration, danr

import XMonad hiding ((|||))

import System.Exit

import XMonad.Layout.NoBorders
import XMonad.Actions.DwmPromote
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Master
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile

import XMonad.Actions.Search
import XMonad.Prompt

import XMonad.Layout.Maximize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.LayoutCombinators 
import XMonad.Util.Font


import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myLayout = 
      avoidStrutsOn [] $ smartBorders $ mkToggle (FULL ?? EOT) $ mkToggle (single MIRROR) 
        (    ResizableTall 1 (3/100) (1/2) []
         ||| mastered (3/100) (1/3) tall
         ||| mastered (3/100) (1/3) (Mirror tall))
         
tall = Tall 1 (3/100) (1/2)

myTerminal =  "urxvt -fn \"xft:terminus-8\" +sb"

myBorderWidth   = 1
myModMask       = mod4Mask

myNumlockMask   = mod2Mask

myWorkspaces    = map show [1..9] 

myNormalBorderColor = "#105577"
myFocusedBorderColor  = "#1892f8"

tyda = searchEngine "tyda" "http://tyda.se/search?form=1&w_lang=&x=0&y=0&w="

myKeys conf@(XConfig {XMonad.modMask = modMask}) = 
  
  let super      = (,) modMask
      shiftSuper = (,) (modMask .|. shiftMask)
  in M.fromList $

      -- Spawn programs
    [ (super xK_r, spawn "urxvt -fn \"xft:Terminus-8\" -rv +sb")
    , (super xK_u, spawn "urxvt -fn \"xft:Terminus-8\" +sb")
    , (super xK_f, spawn "firefox")
    , (super xK_e, spawn "emacs")
    
      -- Search engines
    , (shiftSuper xK_w, promptSearch defaultXPConfig tyda)
    
      -- Spawn dmenu
    , (super xK_l, spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    
      -- Kill window
    , (shiftSuper xK_d, kill)
    
      -- Rotate through the available layout algorithms
    , (super xK_space, sendMessage NextLayout)
    
      --  Reset the layouts on the current workspace to default
    , (shiftSuper xK_space, setLayout $ XMonad.layoutHook conf)
    
      -- Shrink and expand the windows on the non-master area
    , (super xK_v, sequence_ [sendMessage MirrorShrink,sendMessage ShrinkSlave])
    , (super xK_w, sequence_ [sendMessage MirrorExpand,sendMessage ExpandSlave])

      -- Move focus 
    , (super xK_Tab , windows W.focusDown)
    , (super xK_n   , windows W.focusDown)
    , (super xK_t   , windows W.focusUp  )

      -- Swap the focused window and the master window, and focus master (dwmpromote)
    , (super xK_Return, dwmpromote )

      -- Swap the windows
    , (shiftSuper xK_n, windows W.swapDown  )
    , (shiftSuper xK_t, windows W.swapUp    )

      -- Resize the master area
    , (super xK_h, sendMessage Shrink)
    , (super xK_s, sendMessage Expand)
    
	-- Toggle zoom (full) and mirror
    , (super xK_z, sendMessage $ Toggle FULL )
    , (super xK_m, sendMessage $ Toggle MIRROR )

    -- Push window back into tiling
    , (super xK_b, withFocused $ windows . W.sink)

    -- [De]Increment the number of windows in the master area
    , (super xK_comma , sendMessage (IncMasterN 1))
    , (super xK_period, sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , (shiftSuper xK_q, io (exitWith ExitSuccess))

    -- Restart xmonad
    , (super xK_q     , restart "xmonad" True	)	

	
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_k, xK_j, xK_x] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]




------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


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
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , resource  =? "kicker"       --> doIgnore ]


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
-- myLogHook = dynamicLogXmobar

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad defaults --  xmobar $ \conf -> xmonad $ defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
      	normalBorderColor  = myNormalBorderColor,
      	focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook
        -- logHook            = myLogHook
    }


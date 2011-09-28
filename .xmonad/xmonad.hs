{-# LANGUAGE ViewPatterns #-}

-- XMonad configuration, danr

-- Based on skangas' config on github,
-- and on And1's on xmonad wiki

import XMonad hiding ((|||))

import System.Exit

import XMonad.Actions.DwmPromote
import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks

import XMonad.Layout.NoBorders
import XMonad.Layout.Master
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile

import XMonad.Layout.Maximize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.LayoutCombinators 

import XMonad.Hooks.ManageHelpers

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

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

myNormalBorderColor = "#102233"
myFocusedBorderColor  = "#1892f8"

dmenu = "dmenu_run -fn \"-*-terminus-*-*-*-*-*-*-*-*-*-*-*-*\"" 
     ++ " -nb \"#000\" -nf \"#ccc\" -sb \"#333\" -sf \"#66e\" -l 6 -b"

osdColor = "#6060e0"
osdFont = "-*-droid sans mono-*-*-*-*-80-*-*-*-*-*-*-*"

osd time = "osd_cat -d " ++ show time ++ " -p middle -A center -c '" ++ osdColor ++ "'" 
      ++ " -f '" ++ osdFont ++ "'" ++ " -O 2"
      
osdPipe time = "|" ++ osd time

osdText :: String -> Int -> String
osdText t time = "echo \"" ++ t ++ "\"" ++ osdPipe time

dateFormat = "%Y-%m-%d\n%H:%M"
timeFormat = "%H:%M"

killosd = "killall osd_cat; "

osdDate = killosd ++ "date +'" ++ dateFormat ++ "'" ++ osdPipe 2
osdTime = killosd ++ "date +'" ++ timeFormat ++ "'" ++ osdPipe 2
osdAcpi = killosd ++ "acpi | perl -e \"@_ = split(', ',<>); print qq/@_[2]@_[1]/;\"" ++ osdPipe 2

osdspawn s = spawn s >> spawn (osdText (takeWhile (/= ' ') s) 1) 

myKeys conf@(XConfig {XMonad.modMask = modMask}) =

  let just       = (,) 0
      shift      = (,) shiftMask
      super      = (,) modMask
      shiftSuper = (,) (modMask .|. shiftMask)
      
      osdUnbound = do
          let bs = map fst (M.toList (myKeys conf))
              alphabet = ['a'..'z'] ++ ['A'..'Z'] -- ++ "',.[];"
              interpretKey (m,chr . fromEnum -> k)
                | fromEnum m == fromEnum modMask                  = Just k
                | fromEnum m == fromEnum (modMask .|. shiftMask)  = Just (toUpper k)
                | otherwise                                       = Nothing
              keys :: [Char]
              keys = mapMaybe interpretKey bs
              unused = sort (alphabet \\ keys)
              output = unlines ("unbound: ":map (intersperse ' ') (chunk 10 unused))
          liftIO $ writeFile "/home/dan/unbound" keys
          spawn (osdText output 6)
      
  in M.fromList $

      -- Spawn programs
    [ (super xK_r, osdspawn "urxvt -fn \"xft:Terminus-8\" -rv +sb")
    , (super xK_h, osdspawn "urxvt -fn \"xft:Terminus-8\" +sb")
    , (super xK_c, osdspawn "conkeror")
    , (super xK_e, osdspawn "emacs")
      
      -- List the unbound keys
    , (shiftSuper xK_b, osdUnbound)                    
                              
      -- Information on osd
    , (super xK_d, spawn osdDate)
    , (super xK_t, spawn osdTime)
    , (super xK_a, spawn osdAcpi)
      
      -- Take screenshot
    , (super xK_Print, osdspawn "scrot screen_%Y-%m-%d.png")
    
      -- Dmenu
    , (super xK_g, spawn dmenu)
    
      -- Kill window
    , (shiftSuper xK_d, kill)
    
      -- Rotate through the available layout algorithms
    , (super xK_space, sendMessage NextLayout)
    
      --  Reset the layouts on the current workspace to default
    , (shiftSuper xK_space, setLayout $ XMonad.layoutHook conf)
    
      -- Shrink and expand the windows on the non-master area
    , (super xK_v, sequence_ (take 6 $ cycle [sendMessage MirrorShrink,sendMessage ShrinkSlave]))
    , (super xK_w, sequence_ (take 6 $ cycle [sendMessage MirrorExpand,sendMessage ExpandSlave]))

      -- Move focus 
    , (super xK_Tab , windows W.focusDown)
    , (super xK_n   , windows W.focusDown)
    , (super xK_p   , windows W.focusUp  )

      -- Swap the focused window and the master window, and focus master (dwmpromote)
    , (super xK_Return, dwmpromote )

      -- Swap the windows
    , (shiftSuper xK_n, windows W.swapDown  )
    , (shiftSuper xK_p, windows W.swapUp    )

      -- Resize the master area
    , (super xK_b, sendMessage Shrink)
    , (super xK_f, sendMessage Expand)
    
	-- Toggle zoom (full) and mirror
    , (super xK_z, sendMessage $ Toggle FULL )
    , (super xK_m, sendMessage $ Toggle MIRROR )

      -- Push window back into tiling
    , (shiftSuper xK_t, withFocused $ windows . W.sink)

      -- [De]Increment the number of windows in the master area
    , (super xK_comma , sendMessage (IncMasterN 1))
    , (super xK_period, sendMessage (IncMasterN (-1)))

      -- Quit xmonad
    , (shiftSuper xK_q, io (exitWith ExitSuccess))

      -- Restart xmonad
    , (super xK_q,       restart "xmonad" True)		
      
      -- CycleWS
    , (super xK_o,      moveTo Prev NonEmptyWS)
    , (super xK_u,      moveTo Next NonEmptyWS)
    , (shiftSuper xK_o, shiftToPrev >> prevWS)
    , (shiftSuper xK_u, shiftToNext >> nextWS)
    , (super xK_s,      toggleWS)
    , (super xK_i,      moveTo Next EmptyWS)
      
    ] 
    
    ++
    
    let mute = spawn (killosd ++ "amixer sset Master toggle | grep -w 'on\\|off' -o | head -n 1 | perl -e \"print 'Master: '; print <>;\" " ++ osdPipe 1)
        volume s = spawn (killosd ++ "amixer set Master " ++ s ++ " | grep '[0-9]*%' -o | head -n 1 " ++ osdPipe 1) in     
    
    [ -- Media keys : alsamixer
      (just xF86XK_AudioMute,         mute)
    , (just xF86XK_AudioRaiseVolume,  volume "10%+")
    , (just xF86XK_AudioLowerVolume,  volume "10%-")
    , (shift xF86XK_AudioRaiseVolume, volume "2%+")
    , (shift xF86XK_AudioLowerVolume, volume "2%-")
      
      -- Media keys : mpd
    , (just xF86XK_AudioPlay,        spawn "mpc toggle")
    , (just xF86XK_AudioStop,        spawn "mpc stop")
    , (just xF86XK_AudioPrev,        spawn "mpc prev")
    , (just xF86XK_AudioNext,        spawn "mpc next")
      
    ]
    
    ++
    
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

{-    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3    
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_k, xK_j, xK_x] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
-}

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse myMouseBindings
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
      
    -- Shrink and expand slaves (in non master area) with super + scroll wheel  
    , ((modMask, button4), const (sequence_ [sendMessage MirrorExpand,sendMessage ExpandSlave]))
    , ((modMask, button5), const (sequence_ [sendMessage MirrorShrink,sendMessage ShrinkSlave]))
    ]


------------------------------------------------------------------------
-- Window rules:
myManageHook = composeAll
    [ isFullscreen                  --> doFullFloat 
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , resource  =? "kicker"         --> doIgnore 
    ]

------------------------------------------------------------------------
-- Run xmonad
main = xmonad defaults 

defaults = defaultConfig {

    terminal           = myTerminal,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    numlockMask        = myNumlockMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    
    layoutHook         = myLayout,
    manageHook         = myManageHook
                       
  }


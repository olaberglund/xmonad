{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Data.Functor
import Data.Semigroup
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.GridSelect
import XMonad.Actions.WithAll
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), avoidStruts, docks)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Prelude
import XMonad.StackSet as W
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad

modm :: KeyMask
modm = mod4Mask

corner1 = 0xa7

corner2 = 0x60

myWorkspaces :: [String]
myWorkspaces =
  [ "<fn=1>\x31</fn>",
    "<fn=1>\x32</fn>",
    "<fn=1>\x33</fn>",
    "<fn=1>\x34</fn>",
    "<fn=1>\x35</fn>",
    "<fn=1>\x36</fn>",
    "<fn=1>\x37</fn>",
    "<fn=1>\x38</fn>",
    "<fn=1>\x39</fn>"
  ]

myTerminal :: String
myTerminal = "st"

spotify :: String
spotify = "spotify"

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . docks . withSB (statusBarProp "xmobar" (clickablePP myXmobarPP >>= workspaceNamesPP <&> filterOutWsPP [scratchpadWorkspaceTag])) $ myConfig

myConfig =
  def
    { modMask = modm,
      terminal = myTerminal,
      layoutHook = avoidStruts $ smartSpacing 6 myLayout,
      startupHook = setWMName "LG3D",
      handleEventHook = myHandleEventHook,
      manageHook = myManageHook <+> manageHook def,
      focusedBorderColor = "#7d7d7d",
      normalBorderColor = "#000",
      XMonad.workspaces = myWorkspaces
    }
    `additionalKeys` myKeys
    `additionalMouseBindings` myMouseBindings

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ namedScratchpadManageHook scratchpads
    ]

myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> scratchpadFloat)

xmobarToggleCommand :: String
xmobarToggleCommand = "dbus-send --session --dest=org.Xmobar.Control --type=method_call '/org/Xmobar/Control' org.Xmobar.Control.SendSignal \"string:Toggle 0\""

myMouseBindings =
  [ ((mod1Mask, button5), const $ spawn "pamixer --allow-boost -d 2"), -- decrease master volume
    ((mod1Mask, button4), const $ spawn "pamixer --allow-boost -i 2"), -- increase music volume
    ((mod1Mask, button1), const $ spawn "playerctl play-pause"), -- increase music volume
    ((mod1Mask, 8), const $ spawn "playerctl previous"), -- increase music volume
    ((mod1Mask, 9), const $ spawn "playerctl next") -- increase music volume
  ]

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((modm, xK_Return), spawn myTerminal),
    ((modm, xK_q), kill), -- %! Close the focused window
    ((modm .|. shiftMask, xK_q), recomp),
    ((modm, xK_w), dwmpromote),
    -- layout
    ((modm, xK_i), sendMessage (IncMasterN 1)), -- %! Increment the number of windows in the master area
    ((modm, xK_d), sendMessage (IncMasterN (-1))), -- %! Deincrement the number of windows in the master area
    ((modm .|. shiftMask, xK_f), sendMessage (Toggle "Full")), -- %! Rotate through the available layout algorithms
    ((modm, xK_b), sequence_ [spawn xmobarToggleCommand, sendMessage ToggleStruts, toggleWindowSpacingEnabled]), -- %! Rotate through the available layout algorithms
    ((modm, xK_g), goToSelected def),
    -- toggle key layout
    ((mod1Mask, xK_space), spawn "(setxkbmap -query | grep -q \"layout:\\s\\+us\") && setxkbmap se || setxkbmap us; xmodmap /home/ola/.Xmodmap"),
    -- specific programs
    ((modm, xK_space), spawn "firefox"),
    ((modm, xK_f), spawn "flameshot gui"),
    -- music
    ((0, 0x1008FF11), spawn "pamixer --allow-boost -d 2"), -- decrease master volume
    ((0, 0x1008FF13), spawn "pamixer --allow-boost -i 2"), -- increase music volume
    ((0, 0x1008FF12), spawn "pamixer -t"), -- mute music; 0 to tap mult. media key w/o super
    ((0, 0x1008FF14), spawn "playerctl play-pause"), -- increase music volume
    ((0, 0x1008FF16), spawn "playerctl previous"), -- increase music volume
    ((0, 0x1008FF17), spawn "playerctl next"), -- increase music volume

    -- utilities
    ((modm, 0xff7f), spawn "systemctl suspend"), -- shutdown
    ((modm .|. controlMask, 0xff7f), spawn "poweroff"), -- shutdown
    -- ((mod1Mask, xK_a), promptWSGroupAdd def "Name this group: "),
    -- ((mod1Mask, xK_Tab), promptWSGroupView def "Go to group: "),
    -- ((mod1Mask, xK_d), promptWSGroupForget def "Forget group: "),
    -- move cursor without mouse
    ((controlMask, xK_KP_Left), spawn "xdotool mousemove_relative -- -15 0"),
    ((controlMask, xK_KP_Begin), spawn "xdotool mousemove_relative 0 15"),
    ((controlMask, xK_KP_Right), spawn "xdotool mousemove_relative 15 0"),
    ((controlMask, xK_KP_Up), spawn "xdotool mousemove_relative 0 -15"),
    ((controlMask, xK_KP_Enter), spawn "xdotool click 1"),
    ((modm, xK_x), spawn "xmodmap /home/ola/.Xmodmap && xset r rate 200 70"),
    -- white screen
    ((modm .|. shiftMask, xK_w), spawn "sxiv /home/ola/Pictures/white.png"),
    -- lofi
    ((modm .|. controlMask, xK_l), spawn "togglelofi"),
    -- windows
    ((modm .|. controlMask, xK_q), killAll),
    ((modm .|. shiftMask, xK_s), sinkAll),
    -- redshift
    ((modm .|. controlMask, xK_KP_Insert), spawn "rs 0"),
    ((modm .|. controlMask, xK_KP_End), spawn "rs 1"),
    ((modm .|. controlMask, xK_KP_Down), spawn "rs 2"),
    ((modm .|. controlMask, xK_KP_Next), spawn "rs 3"),
    ((modm .|. controlMask, xK_KP_Left), spawn "rs 4"),
    ((modm .|. controlMask, xK_KP_Begin), spawn "rs 5"),
    ((mod1Mask .|. shiftMask, xK_q), spawn "rs 0"),
    ((mod1Mask, xK_q), spawn "rs 1"),
    ((mod1Mask, xK_w), spawn "rs 2"),
    ((mod1Mask, xK_e), spawn "rs 3"),
    ((mod1Mask, xK_r), spawn "rs 4"),
    -- screen layout
    ((modm .|. mod1Mask, xK_KP_End), spawn "~/.screenlayout/fix.sh"),
    ((modm .|. mod1Mask, xK_KP_Down), spawn "~/.screenlayout/horiz-horiz.desktop.sh  ; pkill trayer ; trayer --edge top --align right --monitor 1 --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --alpha 255 --tint 0x333333 --height 25 &"),
    -- workspaces
    ((modm, xK_r), renameWorkspace def),
    ((modm, xK_e), viewEmptyWorkspace),
    ((modm, xK_Tab), toggleWS' ["NSP"]),
    -- scratchpads
    ((modm, corner1), namedScratchpadAction scratchpads "terminal"),
    ((modm, corner2), namedScratchpadAction scratchpads "terminal"),
    ((modm, xK_s), namedScratchpadAction scratchpads "spotify"),
    -- swap
    ((mod1Mask, xK_s), windows $ W.greedyView =<< W.tag . W.workspace . head . W.visible)
  ]
    -- Reordering monitors
    ++ [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
         | (key, sc) <- zip [xK_comma, xK_period] [1, 0], -- was [0..] *** change to match your screen order ***
           (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
       ]
    ++ [ ((m .|. modm, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
         | (i, k) <- zip myWorkspaces [xK_1 ..],
           (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
       ]
  where
    recomp = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

myLayout = toggleLayouts (noBorders Full) tiled
  where
    tiled = renamed [Replace "[]="] $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myXmobarPP :: PP
myXmobarPP =
  def
    { ppCurrent = myCurrent,
      ppHidden = pad . pad . pad . white, -- my3D . white . xmobarBorder "Top" "#FFF" 1,
      ppVisible = myPPVisible,
      ppHiddenNoWindows = myPPNoWindows,
      ppVisibleNoWindows = Just (lowWhite . myPPVisible),
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, _, _, _] -> [ws], -- [ws, l, wins],
      ppExtras = [logTitles id id],
      ppWsSep = "<fc=#bbbbbb><fn=1></fn></fc>",
      ppPrinters = isCurrentNoWindows ?-> const ppCurrentNoWindows
    }
  where
    ppCurrentNoWindows :: WorkspaceId -> String
    ppCurrentNoWindows = myCurrent . lowWhite
    cond ?-> ppr = (asks cond >>= guard) *> asks (ppr . wsPP)

    myCurrent = wrap "<fn=5><fc=#c41449>\xf053</fc></fn> " " <fn=5><fc=#c41449>\xf054</fc></fn>"
    myVisible = wrap "<fn=5><fc=#FFF>\xf053</fc></fn> " " <fn=5><fc=#FFF>\xf054</fc></fn>"
    myPPVisible = myVisible -- pad . pad . const switcheroo -- const $ my3D switcheroo
    myPPNoWindows = wrap "  " "  " . pad . lowWhite -- my3D . lowWhite

    -- colors
    lowWhite, red, white, yellow :: String -> String
    white = xmobarColor "white" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

    isCurrentNoWindows :: WS -> Bool
    isCurrentNoWindows WS {..} = (W.tag wsWS == W.currentTag wsWindowSet) && isNothing (W.peek wsWindowSet)

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "terminal" spawnTerm findTerm scratchpadFloat,
    NS spotify spotify (className =? "Spotify") scratchpadFloat
  ]
  where
    -- add more if necessary

    spawnTerm = myTerminal ++ " -n scratchpad"
    findTerm = resource =? "scratchpad"

scratchpadFloat :: ManageHook
scratchpadFloat = customFloating $ W.RationalRect l t w h
  where
    h = 0.6
    w = 0.6
    t = 0.5 - h / 2
    l = 0.5 - w / 2

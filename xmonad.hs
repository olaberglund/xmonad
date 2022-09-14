{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Data.Functor
import qualified Data.Map.Strict as M
import Data.Semigroup
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.TopicSpace
import XMonad.Actions.WithAll
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Prelude
import XMonad.StackSet as W
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad

-- default constant declarations

myTerminal = "st"

myEditor = "lvim"

spotify = "spotify"

modm :: KeyMask
modm = mod4Mask

corner1 = 0xa7

corner2 = 0x60

-- myWorkspaces :: [String]
-- myWorkspaces =
--   [ "<fn=1>\x31</fn>",
--     "<fn=1>\x32</fn>",
--     "<fn=1>\x33</fn>",
--     "<fn=1>\x34</fn>",
--     "<fn=1>\x35</fn>",
--     "<fn=1>\x36</fn>",
--     "<fn=1>\x37</fn>",
--     "<fn=1>\x38</fn>",
--     "<fn=1>\x39</fn>"
--   ]

awesomeWS :: String -> String -> String
awesomeWS s1 s2 = wrap "<fn=1>" "</fn>" s1 ++ wrap "<fn=1>" "</fn>" s2

myTopicConfig :: TopicConfig
myTopicConfig =
  def
    { topicDirs = tiDirs topicItems,
      topicActions = tiActions topicItems,
      defaultTopicAction = const (pure ()), -- by default, do nothing
      defaultTopic = "1:WEB" -- fallback
    }

topicItems :: [TopicItem]
topicItems =
  [ inHome (awesomeWS "\x31" "web") (spawn "brave"),
    only (awesomeWS "\x32" ""),
    only (awesomeWS "\x33" "xmonad"),
    TI (awesomeWS "\x34" "uni") "lth" spawnShell,
    inHome (awesomeWS "\x35" "com") (spawn "slack"),
    TI (awesomeWS "\x36" "memo") "Work/weknowit/Sweden_Memo/Svenska_Memo_Demo" spawnMemo
    -- TI "dts" ("<fn=1>\x31</fn>" ++ "<fn=1>: web</fn>") spawnShell,
    -- TI "xm-con" ("<fn=1>\x31</fn>" ++ "<fn=1>: web</fn>") (spawnShell *> spawnShellIn "hs/xm")
  ]
  where
    only :: Topic -> TopicItem
    only n = noAction n "./"

    spawnMemo :: X ()
    spawnMemo = spawnShell

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withEasySB (statusBarProp "xmobar" (clickablePP myXmobarPP >>= workspaceNamesPP <&> filterOutWsPP [scratchpadWorkspaceTag])) toggleStrutsKey $ myConfig
  where
    toggleStrutsKey XConfig {modMask = m} = (m, xK_b)

myConfig =
  def
    { modMask = modm,
      terminal = myTerminal,
      layoutHook = spacingWithEdge 6 myLayout,
      startupHook = setWMName "LG3D",
      handleEventHook = myHandleEventHook,
      manageHook = myManageHook <+> manageHook def,
      focusedBorderColor = myLight,
      normalBorderColor = myDark,
      XMonad.workspaces = topicNames topicItems
    }
    `additionalKeys` myKeys

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ namedScratchpadManageHook scratchpads
    ]

myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> scratchpadFloat)

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((modm, xK_Return), spawn myTerminal),
    ((modm, xK_q), kill), -- %! Close the focused window
    ((modm .|. shiftMask, xK_q), recomp),
    ((modm, xK_w), dwmpromote),
    -- layout
    ((modm, xK_i), sendMessage (IncMasterN 1)), -- %! Increment the number of windows in the master area
    ((modm, xK_d), sendMessage (IncMasterN (-1))), -- %! Deincrement the number of windows in the master area
    ((modm, xK_n), sendMessage NextLayout), -- %! Rotate through the available layout algorithms

    -- toggle key layout
    ((mod1Mask, xK_space), spawn "(setxkbmap -query | grep -q \"layout:\\s\\+us\") && setxkbmap se || setxkbmap us"),
    -- specific programs
    ((modm, xK_space), spawn "brave"),
    ((modm, xK_f), spawn "flameshot gui"),
    -- music
    ((0, 0x1008FF11), spawn "pamixer --allow-boost -d 5"), -- decrease master volume
    ((0, 0x1008FF12), spawn "pamixer -t"), -- mute music; 0 to tap mult. media key w/o super
    ((0, 0x1008FF13), spawn "pamixer --allow-boost -i 5"), -- increase music volume
    ((0, 0x1008FF14), spawn "playerctl play-pause"), -- increase music volume
    ((0, 0x1008FF16), spawn "playerctl previous"), -- increase music volume
    ((0, 0x1008FF17), spawn "playerctl next"), -- increase music volume

    -- utilities
    ((modm, 0xff7f), spawn "shutdown now"), -- shutdown

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
    -- screen layout
    ((modm .|. mod1Mask, xK_KP_End), spawn "~/.screenlayout/fix.sh"),
    ((modm .|. mod1Mask, xK_KP_Down), spawn "~/.screenlayout/horiz-horiz.desktop.sh"),
    -- workspaces
    ((modm, xK_r), renameWorkspace def),
    ((modm, xK_e), viewEmptyWorkspace),
    ((modm, xK_Tab), toggleWS' ["NSP"]),
    -- scratchpads
    ((modm, corner1), namedScratchpadAction scratchpads "terminal"),
    ((modm, corner2), namedScratchpadAction scratchpads "terminal"),
    ((modm, xK_s), namedScratchpadAction scratchpads "spotify")
  ]
    -- Reordering monitors
    -- ++ [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
    --      | (key, sc) <- zip [xK_comma, xK_period] [1, 0], -- was [0..] *** change to match your screen order ***
    --        (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    --    ]
    -- ++ [ ((m .|. modm, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
    --      | (i, k) <- zip myWorkspaces [xK_KP_End, xK_KP_Down, xK_KP_Next, xK_KP_Left, xK_KP_Begin, xK_KP_Right, xK_KP_Home, xK_KP_Up, xK_KP_Prior],
    --        (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    --    ]
    -- ++ [ ((m .|. modm, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
    --      | (i, k) <- zip myWorkspaces [xK_1 ..],
    --        (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    --    ]
    ++
    -- The following does two things:
    --   1. Switch topics (no modifier)
    --   2. Move focused window to topic N (shift modifier)
    [ ((modm .|. m, k), f i)
      | (i, k) <- zip (topicNames topicItems) [xK_1 .. xK_9],
        (f, m) <- [(goto, 0), (windows . W.shift, shiftMask)]
    ]
  where
    recomp = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

myLayout = tiled ||| mirrorTiled ||| renamed [Replace "[ ]"] Full ||| threeCol
  where
    threeCol = renamed [Replace "3x[]"] $ magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = renamed [Replace "[]="] $ Tall nmaster delta ratio
    mirrorTiled = renamed [Replace "=[]"] $ Mirror tiled
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = white " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = myCurrent,
      ppHidden = pad . pad . pad . white, -- my3D . white . xmobarBorder "Top" "#FFF" 1,
      ppVisible = myPPVisible,
      ppHiddenNoWindows = myPPNoWindows,
      ppVisibleNoWindows = Just (lowWhite . myPPVisible),
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, _, _, _] -> [ws], -- [ws, l, wins],
      ppExtras = [logTitles id id],
      ppWsSep = "<fc=#bbbbbb><fn=1>\xf715</fn></fc>",
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

myDark = "#000"

myLight = "#7d7d7d"

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

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnEditor :: X ()
spawnEditor = spawnShellCommand myEditor

spawnShellCommand :: String -> X ()
spawnShellCommand s = currentTopicDir myTopicConfig >>= spawnShellInWithCommand s

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminal ++ " --working-directory " ++ dir

spawnShellInWithCommand :: Dir -> String -> X ()
spawnShellInWithCommand dir comm = spawn $ myTerminal ++ " --working-directory " ++ dir ++ " -e " ++ comm

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

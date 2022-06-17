{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Functor
import Data.Semigroup
import XMonad
import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.WithAll
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed
import XMonad.Layout.ThreeColumns
import XMonad.Prelude
import XMonad.StackSet as W
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad

modm :: KeyMask
modm = mod4Mask

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6"] -- keep at an even number < 9

myTerminal :: String
myTerminal = "st"

spotify :: String
spotify = "spotify"

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withEasySB (statusBarProp "xmobar" (clickablePP myXmobarPP >>= workspaceNamesPP <&> filterOutWsPP [scratchpadWorkspaceTag])) toggleStrutsKey $ myConfig
  where
    toggleStrutsKey XConfig {modMask = m} = (m, xK_b)

myConfig =
  def
    { modMask = modm,
      terminal = myTerminal,
      layoutHook = myLayout,
      handleEventHook = myHandleEventHook,
      manageHook = myManageHook <+> manageHook def,
      focusedBorderColor = myLight,
      normalBorderColor = myDark,
      XMonad.workspaces = myWorkspaces
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

    -- windows
    ((modm .|. controlMask, xK_q), killAll),
    ((modm .|. shiftMask, xK_s), sinkAll),
    -- redshift
    ((modm, xK_KP_Insert), spawn "rs 0"),
    ((modm, xK_KP_End), spawn "rs 1"),
    ((modm, xK_KP_Down), spawn "rs 2"),
    ((modm, xK_KP_Next), spawn "rs 3"),
    ((modm, xK_KP_Left), spawn "rs 4"),
    ((modm, xK_KP_Begin), spawn "rs 5"),
    -- workspaces
    ((modm, xK_r), renameWorkspace def),
    ((modm, xK_e), viewEmptyWorkspace),
    -- scratchpads
    ((modm, xK_section), namedScratchpadAction scratchpads "terminal"),
    ((modm, xK_s), namedScratchpadAction scratchpads "spotify")
  ]
    -- Reordering monitors
    ++ [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
         | (key, sc) <- zip [xK_comma, xK_period] [1, 0], -- was [0..] *** change to match your screen order ***
           (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
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
      ppCurrent = invert3D myLight myDark myMed 2,
      ppHidden = my3D . white . xmobarBorder "Top" "#FFF" 1,
      ppVisible = myPPVisible,
      ppHiddenNoWindows = myPPNoWindows,
      ppVisibleNoWindows = Just (const $ myPPNoWindows switcheroo),
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused],
      ppWsSep = "",
      ppPrinters = isCurrentNoWindows ?-> const ppCurrentNoWindows
    }
  where
    ppCurrentNoWindows :: WorkspaceId -> String
    ppCurrentNoWindows = invert3D myLight myDark myMed 2 . lowWhite
    cond ?-> ppr = (asks cond >>= guard) *> asks (ppr . wsPP)

    my3D = create3D myLight myDark myMed 2
    myPPVisible = const $ my3D switcheroo
    myPPNoWindows = my3D . lowWhite
    formatFocused = my3D . pad . matchIcon . ppWindow
    formatUnfocused = const (my3D "")
    create3D lc dc mc w = xmobarBorder "Right" dc w . xmobarBorder "Bottom" dc (w -1) . xmobarBorder "Top" lc w . xmobarBorder "Left" lc w . xmobarColor "white" mc . (' ' :) . (++ " ")
    invert3D lc dc mc w = xmobarBorder "Right" lc w . xmobarBorder "Top" dc w . xmobarBorder "Left" dc w . xmobarColor "white" mc . (' ' :) . (++ " ")
    switcheroo = "<fn=1>\61561</fn>"

    ppWindow :: String -> String
    ppWindow = \w -> if null w then "untitled" else w

    matchIcon :: String -> String
    matchIcon s
      | "NVIM" `isInfixOf` s = "<fn=2>\xe7c5</fn>  " ++ xmobarRaw (shorten 20 $ takeWhile (/= '(') s)
      | "YouTube" `isInfixOf` s = "<fn=2>\xf16a</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Messenger" `isInfixOf` s = "<fn=2>\xf70d</fn>  " ++ s
      | "Facebook" `isInfixOf` s = "<fn=2>\xf09a</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Stack" `isInfixOf` s = "<fn=2>\xf16c</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Spotify" `isInfixOf` s = "<fn=2>\xf1bc</fn>  " ++ s
      | "Twitch" `isInfixOf` s = "<fn=2>\xfa42</fn>  " ++ xmobarRaw (shorten 20 s)
      | "GitHub" `isInfixOf` s = "<fn=2>\xf7a3</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Slack" `isInfixOf` s = "<fn=2>\xf198</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Discord" `isInfixOf` s = "<fn=2>\xfb6e</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Pirate" `isInfixOf` s = "<fn=2>\xfb8a</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Codewars" `isInfixOf` s = "<fn=2>\xe000</fn>  " ++ xmobarRaw (shorten 20 s)
      | "askell" `isInfixOf` s = "<fn=2>\xe61f</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Wikipedia" `isInfixOf` s = "<fn=2>\xfaab</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Firebase" `isInfixOf` s = "<fn=2>\xf6b7</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Dashboard" `isInfixOf` s = "<fn=2>\xfa6d</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Inbox" `isInfixOf` s = "<fn=2>\xf7aa</fn>  " ++ xmobarRaw (shorten 20 s)
      | "Brave" `isInfixOf` s = "<fn=2>\xe743</fn>  " ++ xmobarRaw (shorten 20 s)
      | "st" `isInfixOf` s = "<fn=2>\xf120</fn>  " ++ xmobarRaw "term (" ++ myTerminal ++ ")"
      | otherwise = s

    -- colors
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "white" ""
    djungleYellow = xmobarColor "#d9c731" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myDark = "#000"

myLight = "#7d7d7d"

myMed = "#333"

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

-- /* CSV */
-- a7b6a3,0c231e,78a3ab,597268,6a9198,2e4941,525336,2e585e,16393d

-- /* Array */
-- ["a7b6a3","0c231e","78a3ab","597268","6a9198","2e4941","525336","2e585e","16393d"]

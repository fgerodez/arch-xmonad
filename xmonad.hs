{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import Data.Char (isSpace)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces (renameWorkspaceByName)
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.Minimize
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.TaffybarPagerHints
import XMonad.Hooks.WorkspaceHistory
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Grid
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Prompt.Input ((?+))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
  ( additionalKeysP,
    removeKeysP,
  )
import XMonad.Util.NamedScratchpad
import XMonad.Util.NoTaskbar
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare (filterOutWs)

main :: IO ()
main =
  xmonad
    . docks
    . addEwmhWorkspaceSort (pure $ filterOutWs [scratchpadWorkspaceTag])
    . ewmhFullscreen
    . pagerHints
    . ewmh
    . dynamicProjects []
    $ customConfig

-- | Name of the terminal emulator
customTerminal :: String
customTerminal = "alacritty"

-- | Name of the scratchpad workspace
scratchpadWs :: String
scratchpadWs = "NSP"

-- | XMonad main configuration structure
customConfig =
  def
    { terminal = customTerminal,
      borderWidth = 4,
      modMask = mod5Mask,
      layoutHook = customLayout,
      manageHook = customManageHook,
      handleEventHook = minimizeEventHook,
      logHook = customLogHook,
      startupHook = customStartupHook,
      focusedBorderColor = "#d69131",
      normalBorderColor = "#68502e"
    }
    `additionalKeysP` customKeysP
    `removeKeysP` ["M-e", "M-S-e"]

-- | Custom key bindings & overrides
customKeysP :: [(String, X ())]
customKeysP =
  [ -- Applications
    ("M-a s", spawn "flameshot gui"),
    ("M-a f", spawn "firefox"),
    ("M-a t", spawn customTerminal),
    ("M-a e", spawn "emacsclient -c -n -e '(switch-to-buffer nil)'"),
    ("M-a c", spawn "CM_LAUNCHER=rofi clipmenu"),
    -- Empty workspaces functions
    ("M-S-m", tagToEmptyWorkspace),
    ("M-m", withFocused minimizeWindow),
    -- Scratchpads
    ("M-u k", namedScratchpadAction scratchpads "keepassxc"),
    ("M-u t", namedScratchpadAction scratchpads "term"),
    ("M-u s", namedScratchpadAction scratchpads "signal"),
    -- Workspaces
    ("M-p", createOrSwitchWks),
    ("M-S-p", shiftToWks),
    ("M-r", renameWks),
    -- Workspaces navigation
    ("M-t", toggleWS' [scratchpadWs]),
    ("M-b", spawn "rofi -show windowcd"),
    -- Special keys
    ("<XF86MonBrightnessUp>", spawn "light -A 2"),
    ("<XF86MonBrightnessDown>", spawn "light -U 2"),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"),
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    -- Misc
    ("M-i", withFocused $ windows . W.sink),
    ("M-o", spawn "xset s activate"),
    ("M-z", spawn "rofi -show drun"),
    ("M-g", spawn "rofi -show window"),
    ("M-v", kill1),
    ("M-j", BW.focusUp),
    ("M-k", BW.focusDown)
  ]

-- | List of sequenced actions to execute when xmonad starts
--  ewmhDesktopStartup notify ewmh compliant programs that the
--  protocol is supported by xmonad
customStartupHook :: X ()
customStartupHook =
  sequence_
    [ spawnOnce "systemctl --user start xmonad-session.target",
      spawnOnce "$HOME/.fehbg",
      spawnOnce "xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock"
    ]

-- | Move the pointer to the center of the newly focused window.
--  Filter the NSP workspace from the list exposed through ewmh.
--  Add navigation history.
customLogHook :: X ()
customLogHook = updatePointer (0.5, 0.5) (0.5, 0.5) >> workspaceHistoryHook

-- | Handle specific window placement. ManageHooks are applied right to left
--  1: Apply scratchpad hooks
--  2: Handle Firefox PictureInPicture window
--  3: Float splash / dialogs at the center of the screen
customManageHook :: ManageHook
customManageHook = (floats --> doCenterFloat) <+> namedScratchpadManageHook scratchpads
  where
    floats = isDialog <||> isSplash <||> ((title =? "win0" <||> title =? "win2") <&&> className =? "jetbrains-phpstorm")

-- | List of available window layouts with modifiers
customLayout =
  smartBorders
    . avoidStruts
    . spaced
    . minimize
    . BW.boringAuto
    $ Tall 1 (3 / 100) (1 / 2)
      ||| Simplest
      ||| ThreeColMid 1 (3 / 100) (1 / 2)
      ||| Grid
  where
    spaced = spacingRaw True (Border 0 0 0 0) True (Border 10 10 10 10) True

-- Definition of named scratchpads
scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "keepassxc" "keepassxc" (className =? "KeePassXC") floatAndHide,
    NS "term" "alacritty --class scratchterm" (resource =? "scratchterm") floatAndHide,
    NS "signal" "signal-desktop --use-tray-icon" (className =? "Signal") floatAndHide
  ]
  where
    floatAndHide = doRectFloat (W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)) <+> noTaskbar

-------------------------------------------
-- Workspace action prompts
-------------------------------------------

createOrSwitchWks :: X ()
createOrSwitchWks = workspacesPrompt "create or switch" ?+ (\n -> switchProject (Project n "~/" Nothing))

shiftToWks :: X ()
shiftToWks = workspacesPrompt "shift window" ?+ (\n -> shiftToProject (Project n "~/" Nothing))

renameWks :: X ()
renameWks = workspacesPrompt "rename" ?+ renameWorkspaceByName

-- | Displays a graphical prompt that returns the user choice as a String
runPrompt :: (MonadIO m) => String -> [String] -> m String
runPrompt prompt choices = runProcessWithInput "rofi" ["-dmenu", "-p", prompt] $ unlines choices

-- | Displays a prompt listing the available workspaces
workspacesPrompt :: String -> X (Maybe String)
workspacesPrompt prompt = do
  wks <- gets (sort . map W.tag . filterScratch . W.workspaces . windowset)
  value <- runPrompt prompt wks
  if all isSpace value
    then return Nothing
    else return (listToMaybe . lines $ value)
  where
    filterScratch = filterOutWs [scratchpadWorkspaceTag]

-- -------------------------------------------
-- Helper functions
-- -------------------------------------------

-- | Checks if a window is of type 'splash'
isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

-- -------------------------------------------
-- Custom Layout
-- -------------------------------------------

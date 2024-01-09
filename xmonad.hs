import Data.Char (isSpace)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Monoid
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces (renameWorkspaceByName)
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Minimize
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Prompt.Input ((?+))
import XMonad.StackSet (greedyView)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
  ( additionalKeys,
    additionalKeysP,
  )
import XMonad.Util.NamedScratchpad
import XMonad.Util.NoTaskbar
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare (filterOutWs)
import XMonad.Layout.Accordion (Accordion(Accordion))
import XMonad.Layout.BinaryColumn (BinaryColumn(BinaryColumn))
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Circle (Circle(Circle))
import XMonad.Layout.DragPane (dragPane, DragType (Horizontal))
import XMonad.Layout.TwoPane (TwoPane(TwoPane))
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeColMid))
import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW

main =
  xmonad
    . docks
    . addEwmhWorkspaceSort (pure $ filterOutWs [scratchpadWorkspaceTag])
    . ewmhFullscreen
    . ewmh
    . dynamicProjects []
    $ customConfig

-- | Name of the terminal emulator
customTerminal :: String
customTerminal = "alacritty"

-- | Name of the scratchpad workspace
scratchpadWs :: String
scratchpadWs = "NSP"

scratchWorkspace :: Project
scratchWorkspace = Project "scratch" "~/" Nothing

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
    `additionalKeys` customWsKeys def

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
    ("M-s k", namedScratchpadAction scratchpads "keepassxc"),
    ("M-s t", namedScratchpadAction scratchpads "term"),
    ("M-s s", namedScratchpadAction scratchpads "signal"),
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
    ("M-f", sendMessage $ Toggle FULL),
    ("M-d", spawn "rofi -show drun"),
    ("M-g", spawn "rofi -show window"),
    ("M-c c", kill1),
    ("M-j", BW.focusUp),
    ("M-k", BW.focusDown)
  ]

-- | Use the super key for workspace navigation because altgr + top row
--  is already used for special characters
customWsKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
customWsKeys conf =
  [((mod4Mask, xK_semicolon), sendMessage (IncMasterN (-1)))]
    ++ [ ((m .|. mod4Mask, k), windows $ f i)
         | (i, k) <- zip (workspaces conf) topRow,
           (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]
    ++
    -- mod-{z,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{z,e,r} %! Move client to screen 1, 2, or 3
    [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_z, xK_e, xK_r] [0 ..],
        (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
  where
    topRow = [0x26, 0xe9, 0x22, 0x27, 0x28, 0x2d, 0xe8, 0x5f, 0xe7, 0xe0]

-- | List of sequenced actions to execute when xmonad starts
--  ewmhDesktopStartup notify ewmh compliant programs that the
--  protocol is supported by xmonad
customStartupHook :: X ()
customStartupHook =
  sequence_
  [   spawnOnce "systemctl --user start xmonad-session.target", 
      spawnOnce "$HOME/.fehbg",
      spawnOnce "xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock"
    ]

-- | Move the pointer to the center of the newly focused window.
--  Filter the NSP workspace from the list exposed through ewmh.
--  Add navigation history.
customLogHook :: X ()
customLogHook =
  updatePointer (0.5, 0.5) (0.5, 0.5)
    <+> workspaceHistoryHook

-- | Handle specific window placement. ManageHooks are applied right to left
--  1: Apply scratchpad hooks
--  2: Handle Firefox PictureInPicture window
--  3: Float splash / dialogs at the center of the screen
customManageHook :: ManageHook
customManageHook =
  composeAll
    [ floats --> doCenterFloat,
      isPictureInPicture --> doSideFloat SE <+> doCopyToAll,
      namedScratchpadManageHook scratchpads
    ]
  where
    floats =
      foldr1
        (<||>)
        [ isDialog,
          isSplash,
          (title =? "win0" <||> title =? "win2") <&&> className =? "jetbrains-phpstorm"
        ]
    doCopyToAll = do
      w <- ask
      doF (\ws -> foldr (copyWindow w) ws $ currentWorkspaces ws)

-- | List of available window layouts with modifiers
customLayout =
  smartBorders
    . avoidStruts
    . spaced
    . minimize
    . BW.boringAuto
    . mkToggle (single FULL)
    $ Tall 1 (3 / 100) (1 / 2)
      ||| Full
      ||| ThreeColMid 1 (3/100) (1/2)
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

-- --------------------------------------------
-- Misc themes
-- --------------------------------------------

-- | Configuration for the tabbed layout
tabConfig :: Theme
tabConfig =
  def
    { fontName = "xft:Sans",
      decoHeight = 35
    }

historyBackward = do
  wks <- workspaceHistory
  whenJust (lastWorkspace wks) doView
  where
    lastWorkspace = listToMaybe . reverse
    doView w = windows $ greedyView w

historyForward = do
  wks <- workspaceHistory
  whenJust (firstWorkspace wks) doView
  where
    firstWorkspace = listToMaybe
    doView w = windows $ greedyView w

-------------------------------------------
-- Workspace action prompts
-------------------------------------------

createOrSwitchWks :: X ()
createOrSwitchWks =
  workspacesPrompt "create or switch"
    ?+ (\name -> switchProject (Project name "~/" Nothing))

shiftToWks :: X ()
shiftToWks =
  workspacesPrompt "shift window"
    ?+ (\name -> shiftToProject (Project name "~/" Nothing))

renameWks :: X ()
renameWks = workspacesPrompt "rename" ?+ renameWorkspaceByName

-- -------------------------------------------
-- Helper functions
-- -------------------------------------------

-- | Displays a graphical prompt that returns the user choice as a String
runPrompt :: MonadIO m => String -> [String] -> m String
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

-- | Checks if a window is of type 'splash'
isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

isPictureInPicture :: Query Bool
isPictureInPicture = appName =? "Toolkit" <&&> className =? "firefox"

currentWorkspaces :: W.StackSet i l a s sd -> [i]
currentWorkspaces = map W.tag . W.workspaces

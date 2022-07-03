{- | PORTIONS OF CODE AND/OR COMMENTS, UNICODE CHARACTERS, HEX CODES, ETC. FROM THE FOLLOWING SOURCES ARE CURRENTLY IN USE BELOW AS OF WRITING:

https://github.com/alternateved/nixos-config/blob/main/config/xmonad/xmonad.hs
https://github.com/arcolinux/arcolinux-xmonad-polybar/blob/master/etc/skel/.xmonad/xmonad.hs
https://github.com/byorgey/dotfiles/blob/master/xmonad.hs
https://github.com/byorgey/split/blob/master/src/Data/List/Split/Internals.hs
https://github.com/liskin/dotfiles/blob/home/.xmonad/XMonad/Actions/DoNotDisturb.hs
https://github.com/liskin/dotfiles/blob/home/.xmonad/XMonad/Hooks/LayoutHistory.hs
https://github.com/liskin/dotfiles/blob/home/.xmonad/XMonad/Util/My.hs
https://github.com/liskin/dotfiles/blob/home/.xmonad/xmonad.hs
https://github.com/liskin/xmonad-contrib/blob/4b315a82edbebf94daf7e0d2ecef4e65108cccbe/XMonad/Layout/Inspect.hs
https://github.com/liskin/xmonad-contrib/blob/4b315a82edbebf94daf7e0d2ecef4e65108cccbe/XMonad/Layout/SubLayouts.hs
https://gitlab.com/dwt1/dotfiles/-/blob/master/.xmonad/xmonad.hs
https://gitlab.com/slotThe/dotfiles/-/blob/master/xmonad/.config/xmonad/src/xmonad.hs
https://hub.darcs.net/rgm/config-xmonad/browse/lib/My/Launch.hs
https://music.youtube.com/watch?v=0mSGydswh3E&feature=share -- [ Don't Get In My Way • Zack Hemsey • RONIN ]
https://wiki.haskell.org/Xmonad/Config_archive/Brent_Yorgey%27s_darcs_xmonad.hs
https://wiki.haskell.org/Xmonad/Config_archive/adamvo%27s_xmonad.hs
https://www.youtube.com/watch?v=n82TXg7VHu0 -- [ Otis McDonald «» Behind these Closed Doors ]

-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main
  ( main
  ) where

import           Control.Concurrent                  (threadDelay)
import           Control.Monad

import           Data.Char
import           Data.List.Split
import qualified Data.Map                            as M
import qualified Data.Map.Strict                     as StrictMap
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio                          ((%))

import           GHC.Exts
import           Graphics.X11.Types

import           System.Directory                    (getCurrentDirectory)
import           System.Environment
import           System.Exit
import           System.IO.Unsafe                    (unsafePerformIO)

import           XMonad

import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.EasyMotion
import           XMonad.Actions.FindEmptyWorkspace
import           XMonad.Actions.FlexibleManipulate   as Flex
import           XMonad.Actions.GridSelect
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.Launcher
import           XMonad.Actions.Minimize
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.Prefix               (PrefixArgument (Raw),
                                                      usePrefixArgument,
                                                      withPrefixArgument)
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves
import qualified XMonad.Actions.Search               as S
import           XMonad.Actions.SpawnOn
import qualified XMonad.Actions.Submap               as SM
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Actions.TopicSpace
import           XMonad.Actions.UpdateFocus
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.Warp
import           XMonad.Actions.WindowMenu
import           XMonad.Actions.WithAll
import qualified XMonad.Actions.WorkspaceNames       as WN
import           XMonad.Layout.WorkspaceDir

import           XMonad.Hooks.CurrentWorkspaceOnTop
import           XMonad.Hooks.DebugKeyEvents
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.RefocusLast
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.WindowSwallowing

import qualified XMonad.Layout.BoringWindows         as BW
import           XMonad.Layout.Decoration            hiding (CustomShrink)
import           XMonad.Layout.Gaps
import           XMonad.Layout.IfMax
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.Magnifier
import           XMonad.Layout.Minimize
import           XMonad.Layout.MosaicAlt
import           XMonad.Layout.MultiToggle
import qualified XMonad.Layout.MultiToggle           as MT (Toggle (..))
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableThreeColumns
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest              (Simplest (..))
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed                hiding (CustomShrink)
import           XMonad.Layout.TrackFloating
import           XMonad.Layout.WindowNavigation

import qualified XMonad.Prompt                       as P
import           XMonad.Prompt.AppLauncher           as AL
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace

import qualified XMonad.StackSet                     as W

import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState         as XS
import qualified XMonad.Util.Hacks                   as Hacks
import           XMonad.Util.NamedScratchpad
import qualified XMonad.Util.PureX                   as PX
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce
import           XMonad.Util.Ungrab
import           XMonad.Util.WorkspaceCompare

newtype LockdownState =
  LockdownState Bool
  deriving (Typeable, Read, Show)

newtype SPStorage =
  SPStorage (M.Map String Window) -- | Stores dynamic scratchpads as a map of name to window.
  deriving (Read, Show)

instance ExtensionClass LockdownState where
  initialValue = LockdownState False
  extensionType = PersistentExtension

instance ExtensionClass SPStorage where
  initialValue = SPStorage M.empty
  extensionType = PersistentExtension

altMask :: KeyMask
altMask = mod1Mask

chatterino :: String
chatterino = "chatterino"

colorOneShadeShortOfAlarm, colorTrueYellow, colorTrafficYellow, colorAmber, colorAqua, colorBlue, colorGreen, colorIntOr, colorMagenta, colorOrInt, colorRed, colorBlack, colorGray, colorYarg, colorWhite ::
     String
colorOneShadeShortOfAlarm :: String = "#790023"

colorTrafficYellow :: String = "#EFB700"

colorTrueYellow :: String = "#ffff00"

colorAmber :: String = "#ffbf00"

colorAqua :: String = "#00ffff" ----- Aqua; inverse of Red

colorBlue :: String = "#0000ff" ----- True Blue; inverse of True Yellow

colorGreen :: String = "#00ff00" ---- Green; inverse of Magenta

colorIntOr :: String = "#ff4f00" ---- International Orange

colorMagenta :: String = "#ff00ff" -- Magenta; inverse of Green

colorOrInt :: String = "#00b0ff" ---- Inverse of International Orange

colorRed :: String = "#ff0000" ------ Red; inverse of Cyan

colorBlack :: String = "#000000" -- BLACK

colorGray :: String = "#808080" --- GRAY

colorYarg :: String = "#2e2e2e" -- YARG

colorWhite :: String = "#ffffff" -- WHITE

data Color -- | The the Day After Tomorrow Color Pallette.
  = Amber
  | Aqua
  | Background
  | Blue
  | Comment
  | Egnaro
  | Foreground
  | Green
  | Magenta
  | OneShadeShortOfAlarm
  | Orange
  | Red
  | TrafficYellow
  | TrueYellow

colorHex :: Main.Color -> String
colorHex -- Converts a color into a hexidecimal string with a leading \'#\'
 =
  \case
    Amber                -> colorAmber
    Aqua                 -> colorAqua
    Background           -> colorBlack
    Blue                 -> colorBlue
    Comment              -> colorGray
    Egnaro               -> colorOrInt
    Foreground           -> colorWhite
    Green                -> colorGreen
    Magenta              -> colorMagenta
    OneShadeShortOfAlarm -> colorOneShadeShortOfAlarm
    Orange               -> colorIntOr
    Red                  -> colorRed
    TrafficYellow        -> colorTrafficYellow
    TrueYellow           -> colorTrueYellow

colorXmobar :: Main.Color -> String -> String
colorXmobar fg = xmobarColor (colorHex fg) "" -- | Use xmobar escape codes to output a string with the given foreground color.

copyCat ::
     (Eq a, Eq i, Eq s) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyCat n s
  | Just w <- W.peek s = copyWindow w n s
  | otherwise = s

curDirToWorkspacename :: X ()
curDirToWorkspacename = do
  name <- WN.getCurrentWorkspaceName
  when (isNothing name) $ do
    dir <- io getCurrentDirectory
    when (dir /= myHome) $ do
      WN.setCurrentWorkspaceName $ last $ splitOneOf "/" dir

discord :: String
discord = "discord"

doShiftAndGo :: String -> Query (Endo (W.StackSet String l Window ScreenId sd))
doShiftAndGo = doF . liftM2 (.) W.view W.shift

emConf :: EasyMotionConfig
emConf =
  def
    { sKeys =
        PerScreenKeys $
        StrictMap.fromList
          [(0, topRowNumKeysPlusBrackets), (1, [xK_F1 .. xK_F12])]
    , cancelKey = xK_Escape
    , emFont = myFontHuge
    , txtCol = colorGreen
    , borderPx = 4
    , borderCol = colorBlue
    }

floatConfReqHook :: Query (Maybe (Endo WindowSet)) -> Event -> X All
floatConfReqHook mh ConfigureRequestEvent {ev_window = w} = do
  runQuery (join <$> (isFloat -?> mh)) w >>= \case
    Nothing -> mempty
    Just e  -> windows (appEndo e) >> pure (All False)
floatConfReqHook _ _ = mempty

focusNthScreen, focusNthScreenLOL, focusNthScreenWUT ::
     PhysicalScreen -> Bool -> X ()
focusNthScreen n greedy = do
  ws <- maybe mempty screenWorkspace =<< getScreen def n
  whenJust ws $
    PX.defile .
    (if greedy
       then PX.greedyView
       else PX.view)

focusNthScreenLOL n greedier = do
  ws <- maybe mempty screenWorkspace =<< getScreen def n
  whenJust ws $
    PX.defile .
    (if greedier
       then PX.shift <> PX.greedyView
       else PX.view)

focusNthScreenWUT n greedierYet = do
  ws <- maybe mempty screenWorkspace =<< getScreen def n
  whenJust ws $
    PX.defile .
    (if greedierYet
       then viewWith copyCat
       else PX.view)

gimp :: String
gimp = "gimp"

hexchat :: [Char]
hexchat = "hexchat"

hiddenFloatWorkspaceTags, hiddenMinWorkspaceTags :: [[Char]]
hiddenFloatWorkspaceTags = map hiddenFloatWorkspaceOf myVisibleWorkspaces -- The names of the workspaces used to hide floating windows.
  where
    hiddenFloatWorkspaceOf wsTag = wsTag ++ "_hf" -- The workspace used to hide floating windows from a given workspace.

hiddenMinWorkspaceTags = map hiddenMinWorkspaceOf myVisibleWorkspaces -- The names of the workspaces used to hide minimised windows.
  where
    hiddenMinWorkspaceOf wsTag = wsTag ++ "_hm" -- The workspace used to hide minimised windows from a given workspace.

hyperMask :: KeyMask
hyperMask = mod3Mask

keyBindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keyBindings XConfig {..} =
  M.fromList $
        -- ||| KILL COMMANDS |||
  [ ((modMask .|. shiftMask .|. controlMask, xK_minus), kill) -- Closes all copies of currently focused window, and only of that window.
  , ((modMask .|. shiftMask .|. altMask, xK_minus), killAll) --- Closes all windows on current workspace; only those windows, and only those copies.
  , ((modMask .|. shiftMask, xK_minus), kill1) ----------------- Closes currently focused copy of window, and only that copy.
  , ((modMask, xK_Pause), spawn "xkill")
  , ((modMask, xK_r), refresh) ---------------------- Resizes viewed windows to correct size.
  , ( (modMask .|. shiftMask, xK_r)
    , spawn "xmonad --recompile; xmonad --restart" -- Recompiles, restarts xmonad.
     )
    -- ||| FOCUS; LAYOUT, SPACING, ROTATION, ETC. |||
  , ((modMask, xK_o), windows W.focusDown >> up) ----------------- Moves focus to next window.
  , ((modMask, xK_e), windows W.focusUp >> up) ------------------- Moves focus to previous window.
  , ((modMask, xK_k), windows W.focusMaster >> up) --------------- Moves focus to master window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapMaster >> up) -- Swaps focused window and master window.
  , ((modMask .|. shiftMask, xK_o), windows W.swapDown >> up) ---- Swaps focused window with next window.
  , ((modMask .|. shiftMask, xK_e), windows W.swapUp >> up) ------ Swaps focused window with previous window.
        -- | Layout algorithms
  , ((modMask .|. shiftMask, xK_Tab), setLayout layoutHook) ------- Resets layouts on current workspace to default.
  , ((modMask .|. controlMask, xK_Tab), sendMessage FirstLayout) -- Cycles immediately to first layout algorithm.
  , ((modMask, xK_Tab), sendMessage NextLayout) ------------------- Rotates through available layout algorithms.
        -- | Layout changes
  , ((modMask, xK_u), sendMessage Expand >> up) ------------------- Expands master area.
  , ((modMask, xK_a), sendMessage Shrink >> up) ------------------- Shrinks master area.
  , ((modMask, xK_apostrophe), sendMessage (IncMasterN 1) >> up) -- Increments number of windows in master area.
  , ( (modMask .|. shiftMask, xK_apostrophe)
    , sendMessage (IncMasterN (-1)) >> up ------------------------- Deincrements number of windows in master area.
     )
  , ((hyperMask, xK_a), withFocused (sendMessage . expandWindowAlt))
  , ((hyperMask, xK_u), withFocused (sendMessage . shrinkWindowAlt))
  , ((hyperMask, xK_o), withFocused (sendMessage . tallWindowAlt))
  , ((hyperMask, xK_e), withFocused (sendMessage . wideWindowAlt))
  , ((hyperMask, xK_space), sendMessage resetAlt)
        -- | Spacing
  , ((modMask .|. hyperMask, xK_a), decWindowSpacing 4) -- Decreases window spacing.
  , ((modMask .|. hyperMask, xK_o), incWindowSpacing 4) -- Increases window spacing.
  , ((modMask .|. hyperMask, xK_e), decScreenSpacing 4) -- Decreases screen spacing.
  , ((modMask .|. hyperMask, xK_u), incScreenSpacing 4) -- Increases screen spacing.
        -- | Rotations
  , ((modMask, xK_backslash), rotSlavesDown) ----- Rotates all windows exlusive of master down, while maintaining focus.
  , ((modMask, xK_equal), rotSlavesUp) ----------- Rotates all windows exlusive of master up, while maintaining focus.
  , ((altMask, xK_Tab), rotAllDown) -------------- Rotates all windows down, while maintaining focus.
  , ((altMask .|. shiftMask, xK_Tab), rotAllUp) -- Rotates all windows up, while maintaining focus.
          -- | Layout reflects; toggles, etc.
  , ((modMask .|. shiftMask, xK_space), sendMessage (MT.Toggle NBFULL)) ------- Toggles noborder/full layout.
  , ((modMask .|. controlMask, xK_x), sendMessage $ MT.Toggle REFLECTX)
  , ((modMask .|. controlMask, xK_y), sendMessage $ MT.Toggle REFLECTY)
  , ((modMask, xK_i), sendMessage $ MT.Toggle MIRROR)
  , ((modMask .|. controlMask, xK_space), sendMessage (MT.Toggle NOBORDERS)) -- Toggles borders.
  , ((modMask, xK_space), sendMessage ToggleStruts) --------------------------- Toggles struts.
  , ((modMask, xK_grave), viewEmptyWorkspace)
  , ((modMask .|. shiftMask, xK_p), tagToEmptyWorkspace)
  , ((hyperMask, xK_Home), withFocused (sendMessage . mergeDir id) >> up)
  , ((hyperMask, xK_Insert), withFocused (sendMessage . MergeAll) >> up)
  , ((hyperMask, xK_x), withFocused (sendMessage . UnMerge) >> up)
  , ((hyperMask, xK_semicolon), sendMessage $ pullGroup L)
  , ((hyperMask, xK_q), sendMessage $ pullGroup U)
  , ((hyperMask, xK_j), sendMessage $ pullGroup D)
  , ((hyperMask, xK_k), sendMessage $ pullGroup R)
          -- | Sinks
  , ((0, xK_Pause), withFocused (windows . W.sink) >> up) -- Pushes focused window back into tiling.
  , ( (altMask, xK_Pause)
    , withFocused (windows . W.sink) >> warpToCenter ------- Pushes all windows on current workspace back into tiling.
     )
    -- | WORKSPACE/SCREEN FOCUS CHANGES
  , ((modMask, xK_Left), prevWS >> warpToCenter >> up)
  , ((modMask, xK_Right), nextWS >> warpToCenter >> up)
  , ((modMask .|. shiftMask, xK_Left), swapTo Prev >> warpToCenter >> up)
  , ((modMask .|. shiftMask, xK_Right), swapTo Next >> warpToCenter >> up)
  , ((modMask .|. hyperMask, xK_Tab), nextScreen >> warpToCenter >> up)
  , ((modMask .|. altMask, xK_Tab), prevScreen >> warpToCenter >> up)
  , ( (modMask .|. hyperMask, xK_space)
    , setLayout layoutHook >> WN.setCurrentWorkspaceName "")
    -- WORKSPACE DIR AND NAME
  , ((hyperMask, xK_c), changeDir xpConfig >> curDirToWorkspacename)
  , ((hyperMask, xK_v), WN.renameWorkspace xpConfig)
          -- | Easy Motion
  , ( (modMask .|. altMask, xK_grave)
    , selectWindow emConf >>= (`whenJust` windows . W.focusWindow))
          -- | Window Menu
  , ((altMask .|. controlMask, xK_Tab), windowMenu)
          -- | Window Prompt
  , ((modMask .|. shiftMask, xK_g), windowPrompt def Goto allWindows)
  , ((modMask .|. shiftMask, xK_b), windowPrompt def Bring allWindows)
  , ((modMask .|. altMask, xK_f), nextMatchWithThis Forward className)
  , ((modMask .|. altMask, xK_b), nextMatchWithThis Backward className)
          -- | Terminals
  , ((modMask, xK_Return), spawn terminal) -------------- Default terminal (Main)
  , ((modMask .|. shiftMask, xK_Return), unGrab >> spawn myOtherTerminal) -- myOtherTerminal  (VARIABLES)
          -- | Other variables
  , ((modMask, xK_x), spawn myBrowser) ---------------------- myBrowser
  , ((modMask, xK_y), spawn myEditor) ----------------------- myEditor
  , ((modMask, xK_t), spawn myFileManager) ------------------ myFileManager
  , ((modMask .|. altMask, xK_t), spawn myRootFileManager) -- myRootFileManager
          -- | Various rofi tools, Grid Select, etc.
  , ((modMask, xK_semicolon), unGrab >> spawn rofiDrun) -- rofi, "run" mode; a prompt, where entries therein are named according the programs themselves--as their names would occur in a terminal, for example.
  , ((modMask, xK_q), unGrab >> spawn rofiRun) ----------- rofi, "drun" mode; a prompt, where entries therein use the apps' respective names as they would appear on a normal menu.
  , ((modMask, xK_Up), goToSelected mygridConfig) ----- Grid Select; go to,
  , ((modMask, xK_Down), bringSelected mygridConfig) -- bring to.
          -- | Other Spawns
  , ((modMask, xK_c), unGrab >> spawn chatterino) -- Chatterino
  , ((modMask, xK_d), unGrab >> spawn discord) ----- Discord
  , ((modMask, xK_m), unGrab >> spawn ytMusic) ----- YouTube Music
  , ((modMask, xK_g), unGrab >> spawn gimp) -------- GIMP
  , ((hyperMask, xK_y), unGrab >> spawn youTube) --- YouTube
  , ((modMask .|. altMask, xK_m), unGrab >> spawn messages) ------- Messages App (desktop app for Google Messages; for SMS on desktop via link to phone)
  , ((0, xK_Print), spawn "scrot") --------------- Spawns scrot (in order to take a screenshot (of everything on any/all screen(s))).
  , ((hyperMask, xK_Print), withPrefixArgument takeScreenshot) -- Spawns scrot: click, hold and drag mouse to select area; release to screenshot.
  , ((controlMask, xK_Print), spawn "scrot -s") -- Spawns scrot: click, hold and drag mouse to select area; release to screenshot.
          -- | [Named Scratchpads]
  , ( (modMask .|. altMask, xK_p)
    , namedScratchpadAction scratchpads "amixer" >> up)
  , ((modMask, xK_j), unGrab >> namedScratchpadAction scratchpads "htop" >> up)
  , ((modMask .|. altMask, xK_space), currentTopicAction topicConfig)
  , ((modMask .|. altMask, xK_slash), curDirToWorkspacename)
  -- , ((modMask, xK_f), SM.submap $ searchEngineMap $ S.promptSearch P.def)
  -- , ((modMask, xK_f), SM.submap $ searchEngineMap $ S.promptSearch prompt)
  , ((modMask, xK_f), SM.submap $ searchEngineMap $ S.promptSearch xpConfig)
  , ((modMask .|. shiftMask, xK_f), SM.submap $ searchEngineMap S.selectSearch)
  , ((modMask, xK_n), switchProjectPrompt prompt)
  , ((modMask .|. shiftMask, xK_n), shiftToProjectPrompt prompt)
          -- | The following keybind runs an xrandar script in order to set my external, larger monitor as the primary display, and the screen on my laptop itself as secondary (screens 0 and 1 respectively).
          -- | This script also defines the screens' respective resolutions and rotations, and their positions relative to each other respecting the combined resolution of both.
  , ( (modMask .|. shiftMask .|. controlMask, xK_Print)
    , spawn
        "xrandr --output eDP-1 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")
          -- | A baby seal walks into a club:
  , ( (modMask .|. shiftMask, xK_Escape)
    , confirmPrompt
        prompt
        "that, 'I, [insert name here], [DATA EXPUNGED] ...lol!'" $
      io exitSuccess -- Quits xmonad.
     )
  ] ++
        -- | Deez:
  [ ((modMask .|. m, k), focusNthScreen i greedy >> warpToCenter >> up)
  | (i, k) <- zip [0 ..] [xK_comma, xK_period]
  , (m, greedy) <- [(0, False), (altMask, True)]
  ] ++
  [ ((modMask .|. m, k), focusNthScreenLOL i greedier >> warpToCenter >> up)
  | (i, k) <- zip [0 ..] [xK_comma, xK_period]
  , (m, greedier) <- [(0, False), (altMask .|. controlMask, True)]
  ] ++
  [ ((modMask .|. m, k), focusNthScreenWUT i greedierYet >> warpToCenter >> up)
  | (i, k) <- zip [0 ..] [xK_comma, xK_period]
  , (m, greedierYet) <- [(0, False), (altMask .|. shiftMask, True)]
  ] ++
  [ ( (m .|. modMask, key)
    , screenWorkspace sc >>= flip whenJust (PX.defile . f) >> warpToCenter >> up)
  | (key, sc) <- zip [xK_comma, xK_period] [0 ..]
  , (f, m) <- [(PX.shift, shiftMask), (PX.shift <> PX.view, hyperMask)]
  ] ++
  [ ((m, k), PX.defile f >> warpToCenter >> up)
  | (i, k) <- zip (drop 12 workspaces) topRowNumKeysPlusBrackets
  , (f, m) <-
      [ (PX.shift i, modMask .|. shiftMask)
      , (PX.view i, modMask)
      , (PX.shift i <> PX.view i, modMask .|. hyperMask)
      , (viewWith copyCat i, modMask .|. controlMask .|. shiftMask)
      ]
  ] ++
  [ ((m, k), PX.defile f >> warpToCenter >> up)
  | (i, k) <- zip workspaces [xK_F1 .. xK_F12]
  , (f, m) <-
      [ (PX.shift i, modMask .|. shiftMask)
      , (PX.view i, modMask)
      , (PX.shift i <> PX.view i, modMask .|. hyperMask)
      , (viewWith copyCat i, modMask .|. controlMask .|. shiftMask)
      ]
  ] ++
  [ ((modMask .|. controlMask, k), windows $ swapWithCurrent i)
  | (i, k) <- zip (drop 12 workspaces) topRowNumKeysPlusBrackets
  ] ++
  [ ((modMask .|. controlMask, k), windows $ swapWithCurrent i)
  | (i, k) <- zip workspaces [xK_F1 .. xK_F12]
  ] ++
  [ ((m, k), PX.defile f)
  | (i, k) <- zip (drop 12 workspaces) topRowNumKeysPlusBrackets
  , (f, m) <- [(viewWith copyCat i, modMask .|. controlMask .|. shiftMask)]
  ] ++
  [ ((m, k), PX.defile f)
  | (i, k) <- zip workspaces [xK_F1 .. xK_F12]
  , (f, m) <- [(viewWith copyCat i, modMask .|. controlMask .|. shiftMask)]
  ] ++
  [ ((m .|. modMask .|. altMask, k), windows $ f i)
  | (i, k) <- zip (drop 12 workspaces) topRowNumKeysPlusBrackets
  , (f, m) <- [(W.greedyView, 0)]
  ] ++
  [ ((m .|. modMask .|. altMask, k), windows $ f i)
  | (i, k) <- zip workspaces [xK_F1 .. xK_F12]
  , (f, m) <- [(W.greedyView, 0)]
  ]

launcherConfig :: LauncherConfig
launcherConfig =
  LauncherConfig
    {pathToHoogle = "/home/ocelot/.cabal/bin/hoogle", browser = myBrowser}

messages, myBrowser :: String
messages =
  "/opt/brave.com/brave/brave-browser --profile-directory=Default --app-id=hpfldicfbfomlpcikngkocigghgafkph"

myBrowser = "brave-browser"

myColorizer :: Window -> Bool -> X (String, String)
myColorizer =
  colorRangeFromClassName
    (0x22, 0x22, 0x22) -- Lowest inactive BG
    (0xdd, 0xdd, 0xdd) -- Highest inactive BG
    (0x00, 0x00, 0x00) -- Active BG
    (0x00, 0x00, 0x00) -- Inactive FG
    (0xff, 0x00, 0xff) -- Active FG

myEditor, myFileManager, myFontHuge, myFontMeh :: String
myEditor = "emacsclient -a '' -c "

myFileManager = "thunar"

myFloatConfReqManageHook :: Query (Maybe (Endo WindowSet))
myFloatConfReqManageHook =
  composeAll
    [ className =? "Steam" -?> doFloat -- prevent Steam from moving its floats to primary screen
    ]

myFontHuge = "xft:B612:size=32"

myFontMeh = "xft:B612:size=8"

{-# NOINLINE myHome #-}
myHome :: String
myHome = unsafePerformIO $ getEnv "HOME"

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig {XMonad.modMask = modm} =
  M.fromList
  -- | MULTIMEDIA KEYS
    [ ((modm, xK_KP_Divide), spawn "amixer -q set Master toggle") -- Mute volume
    , ((modm, xK_KP_Subtract), spawn "amixer -q set Master 5%-") --- Decrease volume
    , ((modm, xK_KP_Add), spawn "amixer -q set Master 5%+") -------- Increase volume
    , ( (modm .|. controlMask, xK_l)
      , launcherPrompt def $ defaultLauncherModes launcherConfig)
    , ((scrollMask, xK_Pause), spawn "playerctl play-pause")
    , ((scrollMask, xK_Page_Up), spawn "playerctl next")
    , ((scrollMask, xK_Insert), spawn "playerctl previous")
    , ((scrollMask, xK_Print), spawn "playerctl stop")
    , ((scrollMask, xK_BackSpace), spawn "xkill")
      -- | CONTROL + SHIFT KEYS
    , ((controlMask .|. shiftMask, xK_Escape), spawn "xfce4-taskmanager")
      -- | MINIMIZE, MAXIMIZE, ETC.
    , ((modm .|. shiftMask, xK_Up), withLastMinimized maximizeWindow)
    , ((modm .|. shiftMask, xK_Down), withFocused minimizeWindow)
    -- | PROMOTE
    , ((modm .|. altMask, xK_k), promote)
    -- | WARP
    , ((modm, xK_z), warpToWindow (1 % 2) (1 % 2)) -- | MOVE POINTER TO CURRENTLY FOCUSED WINDOW
    -- | APP LAUNCHER
    , ((modm .|. altMask, xK_g), AL.launchApp prompt gimp)
    ]

myManageHook :: Query (Endo WindowSet)
myManageHook = manageHook1 <> manageHook2
  where
    manageHook1 =
      composeAll . mconcat $
      [ [isDialog --> doCenterFloat]
      , [isFullscreen --> doFullFloat]
      , [className =? c --> doCenterFloat | c <- myCFloats]
      , [className =? d --> doFloat | d <- myDFloats]
      , [title =? t --> doFloat | t <- myTFloats] -- @doFloat@ forces a window to float.  Useful for dialog boxes and such.
      , [(className =? "firefox" <&&> resource =? "Dialog") --> doFloat] -- Float Firefox Dialog
      , [resource =? r --> doFloat | r <- myRFloats]
      , [resource =? i --> doIgnore | i <- myIgnores]
      , [appName =? "pavucontrol" --> doCenterFloat]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (head workspacesPlusTopics)
        | x <- my1Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 2)
        | x <- my2Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 3)
        | x <- my3Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 4)
        | x <- my4Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 5)
        | x <- my5Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 6)
        | x <- my6Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 7)
        | x <- my7Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 8)
        | x <- my8Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 9)
        | x <- my9Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 10)
        | x <- my10Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 11)
        | x <- my11Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 12)
        | x <- my12Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 13)
        | x <- my13Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 14)
        | x <- my14Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 15)
        | x <- my15Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 16)
        | x <- my16Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 17)
        | x <- my17Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 18)
        | x <- my18Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 19)
        | x <- my19Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 20)
        | x <- my20Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 21)
        | x <- my21Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 22)
        | x <- my22Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 23)
        | x <- my23Shifts
        ]
      , [ (className =? x <||> title =? x <||> resource =? x) -->
        doShiftAndGo (workspacesPlusTopics !! 24)
        | x <- my24Shifts
        ]
      ]
      where
        myCFloats = ["Arandr", "feh", "mpv"]
        myDFloats =
          [ "confirm"
          , "file_progress"
          , "dialog"
          , "download"
          , "error"
          , "Gimp"
          , "notification"
          , "pinentry-gtk-2"
          , "splash"
          , "toolbar"
          ]
        myTFloats = ["Downloads", "Save As...", "Oracle VM VirtualBox Manager"]
        myRFloats = []
        myIgnores = ["desktop_window"]
        my1Shifts = []
        my2Shifts = []
        my3Shifts = []
        my4Shifts = []
        my5Shifts = ["Gimp", "feh"]
        my6Shifts = ["vlc", "mpv"]
        my7Shifts = ["Virtualbox"]
        my8Shifts = ["Thunar"]
        my9Shifts = []
        my10Shifts = []
        my11Shifts = []
        my12Shifts = [myBrowser]
        my13Shifts = [discord]
        my14Shifts = []
        my15Shifts = []
        my16Shifts = []
        my17Shifts = []
        my18Shifts = []
        my19Shifts = []
        my20Shifts = []
        my21Shifts = []
        my22Shifts = []
        my23Shifts = []
        my24Shifts = []
    manageHook2 =
      mconcat
        [ title =? "YouTube Music" --> doShiftAndGo (workspacesPlusTopics !! 14)
        , title =? "YouTube" --> doShiftAndGo (workspacesPlusTopics !! 15)
        , namedScratchpadManageHook scratchpads
        ]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {..} =
  M.fromList
    [ ( (modMask, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster -- mod, mouse-1: Sets window to floating mode; move by dragging.
       )
    , ((modMask, button2), \w -> focus w >> windows W.shiftMaster) -- mod, mouse-2: Raises window to top of stack.
    , ((modMask, button3), Flex.mouseWindow Flex.discrete) ---------- mod, mouse-3: Optional; (subjectively) ostensibly "nicer" than so-called "normal" mouse movement and/or resizing, at least in theory.
    , ( (modMask .|. altMask, button3)
      , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster -- mod+alt, mouse-3: Sets window to floating mode aggressively; resize aggressively by dragging.
       )
    , ((modMask, button4), const $ windows W.swapDown)
    , ((modMask, button5), const $ windows W.swapUp)
    ]

myOtherTerminal, myRootFileManager :: String
myOtherTerminal = "kitty"

myRootFileManager = "sudo thunar"

myVisibleWorkspaces :: [[Char]]
myVisibleWorkspaces = workspacesPlusTopics -- The names of the visible workspaces.

mygridConfig :: GSConfig Window
mygridConfig =
  (buildDefaultGSConfig myColorizer)
    { gs_cellheight = 64
    , gs_cellwidth = 256
    , gs_cellpadding = 5
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font = myFontMeh
    }

prompt :: XPConfig
prompt =
  def
    { P.fgColor = colorWhite
    , P.fgHLight = colorOrInt
    , P.bgColor = colorBlack
    , P.bgHLight = colorIntOr
    , P.font = myFontMeh
    , P.alwaysHighlight = True -- Current best match
    , P.height = 25
    , P.position = P.Top
    , P.promptBorderWidth = 0 -- Fit in with rest of config
    , P.historySize = 50
    , P.historyFilter = P.deleteAllDuplicates
    , P.maxComplRows = Just 5 -- Max rows to show in completion window
    , P.promptKeymap = myXPKeyMap
    , P.searchPredicate = fuzzyMatch
    , P.sorter = fuzzySort
    }
  where
    myXPKeyMap =
      mconcat
        [ fromList
            [ ( (controlMask, xK_w)
              , P.killWord' isSpace XMonad.Hooks.ManageDocks.Prev)
            , ((0, xK_Left), P.moveHistory W.focusUp')
            , ((0, xK_Right), P.moveHistory W.focusDown')
            ]
        , P.vimLikeXPKeymap
        ]

reddit :: S.SearchEngine
reddit = S.searchEngine "reddit" "https://old.reddit.com/r/"

rofiDrun, rofiRun :: String
rofiDrun = "rofi -show drun"

rofiRun = "rofi -show run"

scratchpads :: [NamedScratchpad]
scratchpads =
  zipWith (\o s -> s (customFloating (offsetRR o scratchpadSize))) offsets zSPS
  where
    n = length zSPS
    offsetRR (dl, dt) (W.RationalRect l t w h) =
      W.RationalRect (l + dl) (t + dt) w h
    offsets = zip steps (reverse steps)
    scratchpadSize = W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
    step = 1 / 60
    steps = map (subtract (step * (fromIntegral n / 2))) $ take n [0,step ..]
    zSPS =
      [ NS "amixer" (myOtherTerminal ++ " -e alsamixer") (title =? "alsamixer")
      , NS "htop" (myOtherTerminal ++ " -e htop") (title =? "htop")
      ]

scrollMask :: KeyMask
scrollMask = mod5Mask

searchEngineMap ::
     (Ord a1, Num a1) => (S.SearchEngine -> a2) -> M.Map (a1, KeySym) a2
searchEngineMap method =
  M.fromList
    [ ((0, xK_g), method S.google)
    , ((0, xK_h), method S.hoogle)
    , ((0, xK_w), method S.wikipedia)
    , ((0, xK_i), method S.images)
    , ((0, xK_m), method S.maps)
    , ((0, xK_o), method S.openstreetmap)
    , ((0, xK_s), method S.scholar)
    , ((0, xK_d), method S.duckduckgo)
    , ((0, xK_y), method S.youtube)
    , ((0, xK_t), method S.thesaurus)
    , ((0, xK_r), method reddit)
    ]

switchScreen :: Int -> X ()
switchScreen d = do
  s <- screenBy d
  mws <- screenWorkspace s
  warpToScreen s 0.618 0.618
  case mws of
    Nothing -> return ()
    Just ws -> windows (W.view ws)

takeScreenshot :: PrefixArgument -> X ()
takeScreenshot =
  \case
    Raw 1 -> spawn "scrot -z -u" -- Focused window
    Raw 2 -> spawn "scrot -z" -- Entire screen
  -- The mouse movement via @xdotool@ is needed because otherwise,
  -- if unclutter is active, the pointer will remain hidden.  Uff.
    _ ->
      unGrab *> spawn "xdotool mousemove_relative -- -1 0" *>
      spawn "scrot -z -f -s"

tHSK, tWEB, tDSC, tYTM, tYTB, tEMC, tHXC, tMSG :: Topic
tHSK :: Topic = "<fn=1>\xf120</fn>"

-- tWEB :: Topic = "#1: BRWSR"
tWEB :: Topic = "Γ"

-- tDSC :: Topic = "#2: DSCRD"
tDSC :: Topic = "Δ"

-- tYTM :: Topic = "#3: YTMSC"
tYTM :: Topic = "Θ"

-- tYTB :: Topic = "#4: YTUBE"
tYTB :: Topic = "Λ"

-- tEMC :: Topic = "#5: EMACS"
tEMC :: Topic = "Ξ"

-- tHXC :: Topic = "#6: HXCHT"
tHXC :: Topic = "Π"

-- tMSG :: Topic = "#7: MSSGS"
tMSG :: Topic = "Σ"

topRowNumKeysPlusBrackets :: [KeySym]
topRowNumKeysPlusBrackets =
  [ xK_1
  , xK_2
  , xK_3
  , xK_4
  , xK_5
  , xK_6
  , xK_7
  , xK_8
  , xK_9
  , xK_0
  , xK_bracketleft
  , xK_bracketright
  ]

topicConfig :: TopicConfig
topicConfig =
  def
    { topicDirs = tiDirs topics
    , topicActions = tiActions topics
    , defaultTopicAction = const pass
    , defaultTopic = tHSK
    }
  where
    pass = pure ()

topicPrompt :: XPConfig
topicPrompt =
  prompt
    { P.autoComplete = Just 3000 -- Time is in μs.
    , P.historySize = 0 -- No history in the prompt.
    }

topics :: [TopicItem]
topics =
  [ inHome tWEB $ spawn myBrowser
  , inHome tDSC $ spawn discord
  , inHome tYTM $ spawn ytMusic
  , inHome tYTB $ spawn youTube
  , inHome tEMC $ spawn myEditor
  , inHome tHXC $ spawn hexchat
  , inHome tMSG $ spawn messages
  ]

up :: X ()
up = updatePointer (0.5, 0.5) (0, 0)

viewWith ::
     MonadState XState m
  => (WorkspaceId -> WindowSet -> WindowSet)
  -> WorkspaceId
  -> m Data.Monoid.Any
viewWith viewer tag = do
  itag <- curTag
  when' (tag /= itag) $ do
    modifyWindowSet' (viewer tag)
    Any . (tag ==) <$> curTag
  where
    when' b ma -- | A 'when' that accepts a monoidal return value:
     =
      if b
        then ma
        else return mempty
    curTag = W.tag <$> curWorkspace -- | Get the current tag.
    curWorkspace = W.workspace <$> curScreen -- | Get the current workspace.
    curScreen = withWindowSet' (return . W.current) -- | Get the current screen.
    withWindowSet' = (=<< gets windowset) -- | A generalisation of 'withWindowSet'.
    modifyWindowSet' f = modify $ \xs -> xs {windowset = f (windowset xs)} -- | A generalisation of 'modifyWindowSet'.

warpToCenter :: X ()
warpToCenter =
  gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x 0.5 0.5

workspacesPlusTopics :: [[Char]]
workspacesPlusTopics -- | The names of the visible workspaces/topics.
 = map (('F' :) . show) [(1 :: Int) .. 12] ++ topicNames topics

xpConfig :: XPConfig
xpConfig =
  def
    -- { P.font = "-misc-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
    -- { P.font = myFontMeh
    { P.font = myFontHuge
    , P.bgColor = "#200000"
    , P.fgColor = "#CFCFCF"
    -- , P.height = 22
    , P.height = 64
    , P.position = P.CenteredAt 0.5 0.5
    , P.promptBorderWidth = 2
    , P.showCompletionOnTab = True
    }

youTube, ytMusic :: String
youTube =
  "/opt/brave.com/brave/brave-browser --profile-directory=Default --app-id=agimnkijcaahngcdmfeangaknmldooml"

ytMusic =
  "/opt/brave.com/brave/brave-browser --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"

main :: IO ()
main = do
  let currWS = \t -> "<fn=0>" ++ colorXmobar Green t ++ "</fn>"
  let mySB = statusBarProp "xmobar" (pure myPP)
        where
          myPP = def {ppCurrent = xmobarColor colorBlack colorWhite}
  let notEmpty = \t -> "<fn=0>" ++ colorXmobar Red t ++ "</fn>"
  let order = \(ws:l:t:ex) -> [ws, l] ++ ex ++ [t]
  let showWS = \(W.Workspace tag _ _) -> isVisibleWS tag
        where
          isVisibleWS = not . isHiddenWS -- | Check whether a given workspace is visible or not.
            where
              isHiddenWS wsTag = wsTag `elem` myHiddenWorkspaces -- | Check whether a given workspace is hidden or not.
                where
                  myHiddenWorkspaces =
                    hiddenMinWorkspaceTags ++ hiddenFloatWorkspaceTags -- | The names of all hidden workspaces.
  let urgtWS = colorXmobar Magenta
  let windowCount =
        gets $
        Just .
        show .
        length . W.integrate' . W.stack . W.workspace . W.current . windowset
  let wLayout = \t -> "[ " ++ colorXmobar Egnaro t ++ " ]"
  -- xmproc0 <- spawnPipe "xmobar /home/ocelot/Haskell/.xmobarrc -x 0"
  xmproc0 <- spawnPipe "xmobar /home/ocelot/Haskell/.topXmobarrc -x 0"
  -- xmproc1 <- spawnPipe "xmobar /home/ocelot/Haskell/.xmobarrc -x 1"
  xmproc1 <- spawnPipe "xmobar /home/ocelot/Haskell/.bottomXmobarrc -x 0"
  xmonad $
    withEasySB mySB defToggleStrutsKey .
    withNavigation2DConfig def .
    usePrefixArgument "M3-p" .
    docks .
    additionalNav2DKeys
      (xK_comma, xK_apostrophe, xK_period, xK_p)
      [ (mod4Mask .|. controlMask, windowGo)
      , (mod4Mask .|. controlMask .|. shiftMask, windowSwap)
      ]
      False $
    ewmh
      zConfig
        { logHook =
            dynamicLogWithPP $
            xmobarPP
              { ppCurrent = currWS ---------------------------------- Highlight the current workspace.
              , ppExtras = [windowCount] ---------------------------- Window count on current workspace
              , ppHidden = xmobarColor colorGray "" . wrap "«" "»" -- Hidden workspaces in xmobar
              , ppLayout = wLayout ---------------------------------- Color the active layout name.
              , ppOutput =
                  \x ->
                    hPutStrLn xmproc0 x -- | xmobar on monitor 1
                     >>
                    hPutStrLn xmproc1 x -- | xmobar on monitor 2
              , ppOrder = order -------------------------- Only display the list of workspaces.
              -- , ppSep = xmobarColor colorBlue "" " «» " -- Seperators in xmobar
              , ppSep = xmobarColor colorBlue "" " • " -- Seperators in xmobar
              , ppSort = (. filter showWS) <$> getSortByIndex
              -- , ppTitle = xmobarColor colorTrueYellow ""
              , ppTitle = xmobarColor colorAmber ""
              , ppUrgent = urgtWS -- Highlight any urgent workspaces.
              , ppVisible = notEmpty
              }
        }
  where
    zConfig =
      def
        { terminal =
            "urxvt -fg [100]#00ff00 -bg [100]#000000 -bd [100]#0000ff +sb -bc -uc --font xft:B612-Mono:size=12" -- Sets default terminal to rxvt-unicode.
        , focusFollowsMouse = True ------------------------------------------------------------------------------ Enables option to automatically focus window over which cursor is hovering.
        , logHook = currentWorkspaceOnTop
        , clickJustFocuses = False
        , borderWidth = 3 ----- Set window border width to three pixels;
        , modMask = mod4Mask -- modkey to super (a.k.a. the Windows key).
        , normalBorderColor = colorOneShadeShortOfAlarm -- Set the non-focused windows' borders' and focused window's border's colors to One Shade Short of Alarm,
        , focusedBorderColor = colorIntOr ---------------- and to International Orange respectively.
        , workspaces = workspacesPlusTopics
        , keys = keyBindings <+> myKeys
        , mouseBindings = myMouseBindings
        , layoutHook = zLayoutHook
        , manageHook = myManageHook <+> manageSpawn
        , startupHook =
            do spawn "killall trayer"
               spawn
                 "sleep 1 && trayer -l --transparent true --alpha 0 --tint 000000 --SetPartialStrut true --SetDockType true"
               spawnOnce discord
               spawnOnce "blueman-applet"
               spawnOnce "/usr/local/bin/blueman-start"
               spawnOnce "xfce4-clipman"
               spawnOnce "sh .conky/conky-startup.sh"
               spawnOnce "logoutsound-mx"
               spawnOnce "mx-tweak --tray"
               spawnOnce "sh -c \"sleep 6; exec /usr/bin/apt-notifier"
               spawnOnce "mx-usb-unmounter"
               spawnOnce "nm-applet"
               spawnOnce "xfce4-notes"
               spawnOnce
                 "\"/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1"
               spawnOnce "xfce4-power-manager"
               spawnOnce "system-config-printer-applet"
               spawnOnce "start-pulseaudio-x11"
               spawnOnce "restore-software-brightness"
               spawnOnce "light-locker"
               spawnOnce "\"/usr/bin/startupsound-mx"
               spawnOnce "xdg-user-dirs-update"
               spawnOnce "xdg-user-dirs-gtk-update"
               spawnOnce
                 "\"/usr/bin/gnome-keyring-daemon --start --components=pkcs11"
               spawnOnce "compton --dbus"
               spawnOnce
                 "sh -c 'xrandr --output HDMI-1 --mode 1920x1080 --rate 60.00 --output eDP-1 --mode 1600x900 --rate 59.95 --right-of HDMI-1'"
               spawnOnce
                 "\"/usr/bin/gnome-keyring-daemon --start --components=secrets"
               spawnOnce "volumeicon"
        , handleEventHook =
            swallowEventHookSub (className =? "Kitty") (pure True) <+>
            serverModeEventHook <+>
            focusOnMouseMove <+>
            handleEventHook def <+>
            Hacks.windowedFullscreenFixEventHook <+>
            debugKeyEvents <+>
            minimizeEventHook <+> floatConfReqHook myFloatConfReqManageHook
        } `additionalKeysP`
      [ ("M-s", promptedShift)
      , ("M-S-s", promptedGoto)
            -- | Changing the size of stack windows:
      , ("M3-o", sendMessage MirrorShrink)
      , ("M3-e", sendMessage MirrorExpand)
      , ("M-p", delay >> switchHook toggleWS) -- SWITCH TO PREVIOUS WORKSPACE
      , ("M-C-l l /", toggleLockdown) ---------- LOCKDOWN MODE
      , ("M-C-l =", releaseLockdown)
      , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP) ------- Shifts focused window to next ws
      , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP) -- Shifts focused window to prev ws
      -- | SCREEN CONTROL
      , ("M3-<Down>", switchScreen 1)
      , ("M3-S-<Down>", shiftNextScreen)
      ]
      where
        delay = io (threadDelay 0)
          -- | The following lines are needed for named scratchpads:
        nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP")) -- nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
        promptedGoto = workspacePrompt topicPrompt goto
          where
            goto = switchTopic otherTopicConfig -- | Go to a topic.
              where
                otherTopicConfig =
                  def
                    { topicDirs = tiDirs topics
                    , topicActions = tiActions topics
                    , defaultTopicAction = const pass
                    , defaultTopic = tHSK
                    }
                  where
                    pass = pure ()
        promptedShift = workspacePrompt topicPrompt $ windows . W.shift
        releaseLockdown = XS.put (LockdownState False)
        switchHook = withLockdown
          where
            withLockdown act -- | Perform the given action only if not on lockdown
             = do
              LockdownState l <- XS.get
              unless l act
        toggleLockdown = XS.modify (\(LockdownState l) -> LockdownState (not l))
        zLayoutHook =
          avoidStruts $
          gaps (zip [U, D, L, R] (repeat 0)) $ -- | MAKE MANUAL GAP ADJUSTMENT POSSIBLE
          mkToggle1 NBFULL $
          mkToggle1 REFLECTX $
          mkToggle1 REFLECTY $
          mkToggle1 NOBORDERS $ mkToggle1 MIRROR myDefaultLayout
          where
            myDefaultLayout =
              windowNavigation .
              dir .
              refocusLastLayoutHook .
              trackFloating . BW.boringWindows . subTabbed $
              minimize layouts
              where
                dir = workspaceDir myHome
                layouts = named "∀" (fixl $ sub $ acclimate)
                  where
                    acclimate -- | HACK;
                            -- | TECHNICAL: respond physiologically or behaviorally to a change in an environmental factor under controlled conditions.
                     = smartSpacingWithEdge 0 $ IfMax 6 haaaxxxxx magMOSaltS
                    fixl = layoutHintsWithPlacement (0.5, 0.5) . smartBorders
                    fullS = spacingWithEdge 0 $ noBorders Full
                    haaaxxxxx = IfMax 1 fullS $ IfMax 3 rTiledSnoMASTER hax
                      where
                        hax = IfMax 5 rTCsNOmaster rTCMsNOmaster
                    magMOSalt = noMASTERmag mosALT
                    magMOSaltS =
                      smartSpacingWithEdge 0 $
                      spacingRaw
                        False
                        (Border 0 3 3 3)
                        True
                        (Border 3 3 3 3)
                        True
                        magMOSalt
                    mosALT = MosaicAlt M.empty
                    noMASTERmag = magnify 1.5 (NoMaster 3) True
                    ostentationTheme =
                      def
                        { decoHeight = 4
                        , activeColor = colorIntOr
                        , inactiveColor = colorYarg
                        , urgentColor = colorMagenta
                        , activeBorderWidth = 0
                        , inactiveBorderWidth = 0
                        , urgentBorderWidth = 1
                        , activeBorderColor = colorBlack
                        , inactiveBorderColor = colorBlack
                        , urgentBorderColor = colorRed
                        }
                    rTC = ResizableThreeCol nmaster delta fraction modifymfrac
                      where
                        nmaster = 1
                        delta = (3 / 100)
                        fraction = (1 / 2)
                        modifymfrac = []
                    rTCM =
                      ResizableThreeColMid nmaster delta fraction modifymfrac
                      where
                        nmaster = 1
                        delta = (3 / 100)
                        fraction = (1 / 2)
                        modifymfrac = []
                    rTCMsNOmaster = noMASTERmag rTCMs
                    rTCs =
                      smartSpacingWithEdge 0 $
                      spacingRaw
                        False
                        (Border 0 8 8 8)
                        True
                        (Border 8 8 8 8)
                        True
                        rTC
                    rTCMs =
                      smartSpacingWithEdge 0 $
                      spacingRaw
                        False
                        (Border 0 8 8 8)
                        True
                        (Border 8 8 8 8)
                        True
                        rTCM
                    rTCsNOmaster = noMASTERmag rTCs
                    rTiled = ResizableTall nmaster delta ratio modifymfrac -- Default tiling algorithm, except better; partitions the screen into two panes, but where slave windows are resizeable.
                      where
                        nmaster = 1 ----------------------- The default number of windows in the master pane.
                        ratio = 1 / 2 --------------------- Default proportion of screen occupied by master pane.
                        delta = 3 / 100 ------------------- Percentage of screen by which to increment when resizing panes.
                        modifymfrac = []
                    rTiledS =
                      smartSpacingWithEdge 0 $
                      spacingRaw
                        False
                        (Border 0 4 4 4)
                        True
                        (Border 4 4 4 4)
                        True
                        rTiled
                    rTiledSnoMASTER = noMASTERmag rTiledS
                    sub =
                      addTabs CustomShrink ostentationTheme .
                      subLayout [] Simplest

data CustomShrink =
  CustomShrink

instance Show CustomShrink where
  show _ = ""

instance Read CustomShrink where
  readsPrec _ s = [(CustomShrink, s)]

instance Shrinker CustomShrink where
  shrinkIt _ _ = []

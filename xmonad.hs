{- | PORTIONS OF CODE AND/OR COMMENTS FROM THE FOLLOWING SOURCES IN USE BELOW AS OF WRITING:

https://github.com/alternateved/nixos-config/blob/main/config/xmonad/xmonad.hs
https://github.com/arcolinux/arcolinux-xmonad-polybar/blob/master/etc/skel/.xmonad/xmonad.hs
https://github.com/byorgey/split/blob/master/src/Data/List/Split/Internals.hs
https://github.com/liskin/dotfiles/blob/home/.xmonad/XMonad/Util/My.hs
https://github.com/liskin/dotfiles/blob/home/.xmonad/xmonad.hs
https://github.com/liskin/xmonad-contrib/blob/4b315a82edbebf94daf7e0d2ecef4e65108cccbe/XMonad/Layout/Inspect.hs
https://github.com/liskin/xmonad-contrib/blob/4b315a82edbebf94daf7e0d2ecef4e65108cccbe/XMonad/Layout/SubLayouts.hs
https://gitlab.com/dwt1/dotfiles/-/blob/master/.xmonad/xmonad.hs
https://gitlab.com/slotThe/dotfiles/-/blob/master/xmonad/.config/xmonad/src/xmonad.hs
https://wiki.haskell.org/Xmonad/Config_archive/adamvo%27s_xmonad.hs

-}
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
-- | VOCABULARY:
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}


-- | SANITY CHECK SUPPRESSION:
{-# OPTIONS_GHC -Wno-name-shadowing      #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds    #-} -- But if I comment out said top level bindings, then the compiler yells at me about "vArIaBlE NoT In sCoPe"...
{-# OPTIONS_GHC -Wno-unused-local-binds  #-}
--{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
module Main
  ( main
  ) where -- Module --
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------


-- | Imports ---------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------
  -- | Data, etc.
----------------------------------------------------------------------------------------------------------------
import           Control.Arrow                       ((&&&))
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map                            as M
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as StrictMap (fromList)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                            as DS
import           GHC.Exts
import           System.Directory                    (getCurrentDirectory)
import           System.Environment
import           System.Exit
import           System.IO.Unsafe                    (unsafePerformIO)

----------------------------------------------------------------------------------------------------------------
-- | Xmonad
----------------------------------------------------------------------------------------------------------------
import           XMonad
import           XMonad.Prelude
import qualified XMonad.StackSet                     as W
import           XMonad.Util.Run
import           XMonad.Util.Ungrab


----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
  -- | Actions
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.EasyMotion
import           XMonad.Actions.FindEmptyWorkspace
import           XMonad.Actions.FlexibleManipulate   as Flex
import           XMonad.Actions.GridSelect
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.PhysicalScreens
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

  -- | Hooks
import           XMonad.Hooks.CurrentWorkspaceOnTop
import           XMonad.Hooks.DebugKeyEvents
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.RefocusLast            (refocusLastLayoutHook)
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.UrgencyHook

  -- | Layouts
import           XMonad.Layout.Roledex
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.StackTile
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.TwoPane

  -- | Layouts modifiers
import           XMonad.Layout.MultiToggle
import qualified XMonad.Layout.MultiToggle           as MT (Toggle (..))
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect
import           XMonad.Layout.TrackFloating

  -- | Prompts
import qualified XMonad.Prompt                       as P
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace

  -- | Utilities
import           XMonad.Util.DynamicScratchpads
import           XMonad.Util.EZConfig
import qualified XMonad.Util.Hacks                   as Hacks
import           XMonad.Util.Invisible               (Invisible (..))
import           XMonad.Util.NamedScratchpad
import qualified XMonad.Util.PureX                   as PX


-- | Setup -----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

-- | REQUIRED BY MAIN
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
toGroups :: Ord a1 => [(a2, W.Stack a1)] -> Map a1 (W.Stack a1)
toGroups ws =
  M.fromList . map (W.focus &&& id) . nubBy (on (==) W.focus) $ map snd ws
----------------------------------------------------------------------------------------------------------------
toGroupStack :: Ord a =>
                   Map k (W.Stack a) -> W.Stack a -> W.Stack (W.Stack a)
toGroupStack gs st@(W.Stack f ls rs) =
  W.Stack
  (let Just f' = lu f
   in f')
  (mapMaybe lu ls)
  (mapMaybe lu rs)
  where
    wset = DS.fromList (W.integrate st)
    dead = W.filter (`DS.member` wset)
    refocus s
      | f `elem` W.integrate s = W.filter (`elem` W.integrate s) st
      | otherwise = pure s
    gs' = mapGroups (refocus <=< dead) gs
      where
        mapGroups f = M.fromList . map (W.focus &&& id) . mapMaybe f . M.elems
    gset = DS.fromList . concatMap W.integrate . M.elems $ gs'
    lu w
      | w `DS.member` gset = w `M.lookup` gs'
      | otherwise = Just (W.Stack w [] []) -- [SINGLETON GROUPS FOR NEW WINS]


newtype CWOTState = CWOTS String

instance ExtensionClass CWOTState where
  initialValue = CWOTS ""


-- | The @XLike@ typeclass over monads reading @XConf@ values and tracking @XState@ state.
class (MonadReader XConf m, MonadState XState m) =>
      XLike m
  where
  toX :: m a -> X a

instance XLike X where
  toX = id
data Sublayout l a =
  Sublayout
    { delayMess :: Invisible [] (SomeMessage, a)
    , defSL     :: ([Int], l a)
    , subls     :: [(l a, W.Stack a)]
    }
  deriving (Read, Show)

-- | Get group stack for a given stack.
newtype GetGroupStack =
  GetGroupStack (W.Stack Window)

type instance InspectResult GetGroupStack =
     Alt Maybe (GroupStack Window)

type GroupStack a = W.Stack (W.Stack a)

instance InspectLayout GetGroupStack (Sublayout l) Window where
  inspectLayout (GetGroupStack ws) Sublayout {subls} =
    pure $ toGroupStack (toGroups subls) ws

type family InspectResult i

class Monoid (InspectResult i) =>
      InspectLayout i l a
  where
  inspectLayout :: i -> l a -> InspectResult i

instance {-# OVERLAPPABLE #-} Monoid (InspectResult i) =>
                              InspectLayout i l a where
  inspectLayout _ _ = mempty

instance (InspectLayout i l1 a, InspectLayout i l2 a) =>
         InspectLayout i (Choose l1 l2) a where
  inspectLayout i (Choose CL l1 _) = inspectLayout i l1
  inspectLayout i (Choose CR _ l2) = inspectLayout i l2

data Delimiter a =
  Delimiter [a -> Bool]
  deriving ()

data DelimPolicy
  = Drop
  | Keep
  | KeepLeft
  | KeepRight
  deriving (Eq, Show)

data CondensePolicy
  = Condense
  | DropBlankFields
  | KeepBlankFields
  deriving (Eq, Show)

data EndPolicy
  = DropBlank
  | KeepBlank
  deriving (Eq, Show)

data Splitter a =
  Splitter
    { delimiter        :: Delimiter a
    , delimPolicy      :: DelimPolicy
    , condensePolicy   :: CondensePolicy
    , initBlankPolicy  :: EndPolicy
    , finalBlankPolicy :: EndPolicy
    }

data Chunk a
  = Delim [a]
  | Text [a]
  deriving (Show, Eq)

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------
-- | \\\ MAIN \\\
-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0"
  xmproc1 <- spawnPipe "xmobar -x 1"
  xmonad $
    withEasySB mySB defToggleStrutsKey .
    withNavigation2DConfig def .
    withUrgencyHook NoUrgencyHook -- No popups; only bar notifications.
     $
    additionalNav2DKeys
      (xK_comma, xK_apostrophe, xK_period, xK_p)
      [ (mod4Mask .|. controlMask, windowGo)
      , (mod4Mask .|. controlMask .|. shiftMask, windowSwap)
      ]
      False $
    ewmh
    myConfig
        { logHook =
            dynamicLogWithPP $

            xmobarPP
              { ppOutput =
                  \x ->
                    hPutStrLn xmproc0 x -- | xmobar on monitor 1
                     >>
                    hPutStrLn xmproc1 x -- | xmobar on monitor 2

              , ppCurrent = xmobarColor colorGreen "" . wrap "[" "]" -- Current workspace in xmobar
              , ppTitle = xmobarColor colorGreen "" ------------------- Title of active window in xmobar
              , ppVisible = xmobarColor colorRed "" . wrap "(" ")" ---- Visible but not current workspace
              , ppHidden = xmobarColor colorGray "" . wrap "{" "}" ---- Hidden workspaces in xmobar
              , ppUrgent = xmobarColor colorMagenta colorGreen -------- Urgent workspace
              , ppSep = xmobarColor colorBlue "" " :: " --------------- Seperators in xmobar
              , ppExtras = [windowCount] ------------------------------ Window count on current workspace
              , ppOrder = \(ws:l:t:ex) -> [ws, l] ++ ex ++ [t]

              }

        }

        where
          logHook = historyHook
          mySB = statusBarProp "xmobar" (pure myPP)
          myPP = def {ppCurrent = xmobarColor colorBlack colorWhite}
          windowCount =
            gets $
            Just .
            show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
          myConfig =  def
            { terminal = "kitty" -------- Sets default terminal to kitty.
            , focusFollowsMouse = True -- Enables option to automatically focus window over which cursor is hovering.
            , logHook =  currentWorkspaceOnTop

            , clickJustFocuses = False
            , borderWidth = 2 ----- Set window border width to two pixels;
            , modMask = mod4Mask -- modkey to super (a.k.a. the Windows key).
            , normalBorderColor = colorBlack ----- Set the non-focused windows' borders' and focused window's border's colors to black,
            , focusedBorderColor = colorMagenta -- and to magenta respectively.

            , workspaces = workspacesPlusTopics

                                   -- Keybinds & mouse bindings:
            , keys = keyBindings <+> myKeys
            , mouseBindings = myMouseBindings


                          -- Hooks & layouts:
            , layoutHook = smartSpacingWithEdge 3 myLayoutHook
            , manageHook =
              mconcat
              [ className =? "Gimp" --> doFloat
              , appName =? "pavucontrol" --> doFloat
              , resource =? "desktop_window" --> doIgnore

              -- ] <+> myManageHook <+> myOtherManageHook <+> manageSpawn <+>
              ] <+> myManageHook <+> manageSpawn <+>
              namedScratchpadManageHook myScratchPads

            , startupHook =
              do spawn "feh --bg-scale /home/ocelot/Documents/green-triangle_LOL.png"
                 spawn "killall trayer"
                 spawn "sleep 1 && trayer --SetPartialStrut true --SetDockType true"

            , handleEventHook =
              focusOnMouseMove <+>
              handleEventHook def <+>
              Hacks.windowedFullscreenFixEventHook <+>
              debugKeyEvents
            } `additionalKeysP`
              [("M-s", promptedShift), ("M-S-s", promptedGoto)]


          workspacesPlusTopics = map (('F' :) . show) [(1 :: Int) .. 12] ++ topicNames topics



          -- WINDOW MANIPULATIONS
          myManageHook = composeAll . mconcat $
              [ [isDialog --> doCenterFloat]
              , [isFullscreen -->  doFullFloat]
              , [className =? c --> doCenterFloat | c <- myCFloats]
              , [className =? d --> doFloat | d <- myDFloats]
              , [title =? t --> doFloat | t <- myTFloats] -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.

              , [(className =? "firefox" <&&> resource =? "Dialog") --> doFloat]  -- Float Firefox Dialog

              , [resource =? r --> doFloat | r <- myRFloats]
              , [resource =? i --> doIgnore | i <- myIgnores]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (head workspacesPlusTopics) | x <- my1Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 2) | x <- my2Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 3) | x <- my3Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 4) | x <- my4Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 5) | x <- my5Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 6) | x <- my6Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 7) | x <- my7Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 8) | x <- my8Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 9) | x <- my9Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 10)| x <- my10Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 11) | x <- my11Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 12) | x <- my12Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 13) | x <- my13Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 14) | x <- my14Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 15) | x <- my15Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 16) | x <- my16Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 17) | x <- my17Shifts]
              , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (workspacesPlusTopics !! 18) | x <- my18Shifts]
              ]
              where
              myCFloats = ["Arandr", "feh", "mpv"]
              myDFloats = ["confirm" , "file_progress" , "dialog" , "download" , "error" , "Gimp" , "notification" , "pinentry-gtk-2" , "splash" , "toolbar"]
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
              my14Shifts = [ytMusic]
              my15Shifts = []
              my16Shifts = []
              my17Shifts = []
              my18Shifts = []

          doShiftAndGo = doF . liftM2 (.) W.view W.shift

          -- | Prompt for going to topics that are not available via direct keybindings.
          promptedGoto :: X ()
          promptedGoto = workspacePrompt topicPrompt goto

          -- | Go to a topic.
          goto = switchTopic topicConfig

          -- | Prompt for shifting windows to topics that are not available via direct keybindings.
          promptedShift = workspacePrompt topicPrompt $ windows . W.shift
          topicPrompt =
            prompt
            { P.autoComplete = Just 3000 -- Time is in μs.
            , P.historySize = 0 -- No history in the prompt.
            }

          topics =
            [ inHome tWEB $ spawn myBrowser

            , inHome "2: DSCRD" $
              spawn
              discord

            , inHome "3: YTMSC" $
              spawn ytMusic

            , inHome "4: YTUBE" $
              spawn youTube

            , inHome "5: EMACS" $
              spawn myEditor

            , inHome "6: HXCHT" $
              spawn
              "hexchat"

            ]


          topicConfig =
            def
            { topicDirs = tiDirs topics
            , topicActions = tiActions topics
            , defaultTopicAction = const pass
            , defaultTopic = tHSK
            }

          pass = pure ()

          myBrowser = "brave-browser"
          myEditor = "emacsclient -a '' -c "
          myFileManager = "doublecmd"
          myOtherTerminal =
            "urxvt -fg [100]#ffff00 -bg [100]#000000 -bd [100]#0000ff +sb -bc -uc"

          discord = "discord"
          chatterino = "chatterino"
          ytMusic = "/opt/brave.com/brave/brave-browser --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"
          youTube = "/opt/brave.com/brave/brave-browser --profile-directory=Default --app-id=agimnkijcaahngcdmfeangaknmldooml " --app-launch-url-for-shortcuts-menu-item=https://www.youtube.com/feed/explore?feature=app_shortcuts" "

          rofiRun = "rofi -show run"
          rofiDrun = "rofi -show drun"


          -- | HACK -- @lockMask@, used anywhere else in this file respecting keybinds, refers to the entry under @Xmodmap@ called @Lock@, which, in my case, is bound to @Caps_Lock@.

          altMask = mod1Mask
          hyperMask = mod3Mask
          scrollMask = mod5Mask



          -- | MODM KEYBINDINSS
          ------------------------------------------------------------------------------------------------------------------
          ------------------------------------------------------------------------------------------------------------------
          myKeys XConfig {XMonad.modMask = modm} =
            M.fromList

        -- | MULTIMEDIA KEYS
            [ ((modm, xK_KP_Divide), spawn "amixer -q set Master toggle") -- Mute volume
            , ((modm, xK_KP_Subtract), spawn "amixer -q set Master 5%-") --- Decrease volume
            , ((modm, xK_KP_Add), spawn "amixer -q set Master 5%+") -------- Increase volume

            , ((scrollMask, xK_Pause), spawn "playerctl play-pause")
            , ((scrollMask, xK_Page_Up), spawn "playerctl next")
            , ((scrollMask, xK_Insert), spawn "playerctl previous")
            , ((scrollMask, xK_Print), spawn "playerctl stop")

            , ((scrollMask, xK_BackSpace), spawn "xkill")

            -- | CONTROL + SHIFT KEYS
            , ((controlMask .|. shiftMask , xK_Escape ), spawn "xfce4-taskmanager")

            ]

          keyBindings XConfig {..} =
            M.fromList $

-- | KILLS
            [ ((modMask .|. shiftMask .|. controlMask, xK_minus), kill) -- Closes all copies of currently focused window, and only of that window.
            , ((modMask .|. shiftMask .|. altMask, xK_minus), killAll) --- Closes all windows on current workspace; only those windows, and only those copies.
            , ((modMask .|. shiftMask, xK_minus), kill1) ----------------- Closes currently focused copy of window, and only that copy.

-- | REFRESHES
            , ((modMask, xK_r), refresh) ---------------------- Resizes viewed windows to correct size.
            , ( (modMask .|. controlMask .|. shiftMask, xK_r)
              , spawn "xmonad --recompile; xmonad --restart" -- Recompiles, restarts xmonad.
              )

-- | FOCUS CHANGES, SWAPS, ETC.
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

              -- | Layout reflects
            , ((modMask .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)
            , ((modMask .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)


-- | TOGGLES
            , ((modMask .|. shiftMask, xK_space), sendMessage (MT.Toggle NBFULL)) ------- Toggles noborder/full layout.
            , ((modMask .|. controlMask, xK_space), sendMessage (MT.Toggle NOBORDERS)) -- Toggles borders.
            , ((modMask, xK_space), sendMessage ToggleStruts) --------------------------- Toggles struts.


-- | WORKSPACES
            , ((modMask, xK_p), viewEmptyWorkspace)
            , ((modMask .|. shiftMask, xK_p), tagToEmptyWorkspace)


-- | SINKS
            , ((modMask, xK_grave), withFocused (windows . W.sink) >> up) -- Pushes focused window back into tiling.
            , ( (modMask .|. shiftMask, xK_grave)
              , withFocused (windows . W.sink) >> warpToCenter ------------- Pushes all windows on current workspace back into tiling.
              )


-- | PROMPTS, ETC.
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


-- | SPAWNS
              -- | Terminals
            , ((modMask, xK_Return), spawn terminal) -------------- Default terminal (Main)
            , ((modMask, xK_t), unGrab >> spawn myOtherTerminal) -- myOtherTerminal  (VARIABLES)

              -- | Other variables
            , ((modMask, xK_x), spawn myBrowser) ------ myBrowser

            , ((modMask, xK_y), spawn myEditor) ------- myEditor
            , ((modMask, xK_i), spawn myFileManager) -- myFileManager

              -- | Various rofi tools, Grid Select, etc.
            , ((modMask .|. hyperMask, xK_semicolon), unGrab >> spawn rofiDrun) -- rofi, "run" mode; a prompt, where entries therein are named according the programs themselves--as their names would occur in a terminal, for example.
            , ((modMask .|. hyperMask, xK_q), unGrab >> spawn rofiRun) ----------- rofi, "drun" mode; a prompt, where entries therein use the apps' respective names as they would appear on a normal menu.
            , ((modMask, xK_Up), goToSelected mygridConfig) ----- Grid Select; go to,
            , ((modMask, xK_Down), bringSelected mygridConfig) -- bring to.

              -- | Other Spawns:
            , ((modMask, xK_c), unGrab >> spawn chatterino) -- Chatterino
            , ((modMask, xK_d), unGrab >> spawn discord) ----- Discord
            , ((modMask, xK_m), unGrab >> spawn ytMusic) ------- YouTube Music
            , ((0, xK_Print), spawn "scrot") --------------- Spawns scrot (in order to take a screenshot).
            , ((controlMask, xK_Print), spawn "scrot -s") -- Spawns scrot; click and drag mouse to select section of area to screenshot.


-- | SCRATCHPADS
              -- [Named Scratchpads]
            , ((modMask, xK_h), unGrab >> scratchHtop >> up) ------- htop
            , ((hyperMask, xK_p), unGrab >> scratchAlsamixer >> up) -- Alsamixer

              -- | [Dynamic Scratchpads]
            , ( (controlMask .|. hyperMask .|. shiftMask, xK_1)
              , withFocused $ makeDynamicSP "dyn1")
            , ( (controlMask .|. hyperMask .|. shiftMask, xK_2)
              , withFocused $ makeDynamicSP "dyn2")
            , ( (controlMask .|. hyperMask .|. shiftMask, xK_3)
              , withFocused $ makeDynamicSP "dyn3")
            , ( (controlMask .|. hyperMask .|. shiftMask, xK_4)
              , withFocused $ makeDynamicSP "dyn4")
            , ( (controlMask .|. hyperMask .|. shiftMask, xK_5)
              , withFocused $ makeDynamicSP "dyn5")
            , ((controlMask .|. hyperMask, xK_1), spawnDynamicSP "dyn1")
            , ((controlMask .|. hyperMask, xK_2), spawnDynamicSP "dyn2")
            , ((controlMask .|. hyperMask, xK_3), spawnDynamicSP "dyn3")
            , ((controlMask .|. hyperMask, xK_4), spawnDynamicSP "dyn4")
            , ((controlMask .|. hyperMask, xK_5), spawnDynamicSP "dyn5")


-- | TOPICS
            , ((modMask .|. altMask, xK_space), currentTopicAction topicConfig)
            , ((modMask .|. altMask, xK_slash), curDirToWorkspacename)


-- | SEARCH
            , ((modMask, xK_f), SM.submap $ searchEngineMap $ S.promptSearch P.def)
            , ((modMask .|. shiftMask, xK_f), SM.submap $ searchEngineMap S.selectSearch)


-- | DYNAMIC PROJECTS
            , ((modMask, xK_n), switchProjectPrompt prompt)
            , ((modMask .|. shiftMask, xK_n), shiftToProjectPrompt prompt)


-- | OTHER
              -- | The following keybind runs an xrandar script in order to set my external, larger monitor as the primary display, and the screen on my laptop itself as secondary (screens 0 and 1 respectively).
              -- | This script also defines the screens' respective resolutions and rotations, and their positions relative to each other respecting the combined resolution of both.
            , ( (modMask .|. shiftMask .|. controlMask, xK_Print)
              , spawn
                "xrandr --output eDP-1 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")

              -- | A baby seal walks into a club:
            , ( (modMask .|. controlMask .|. altMask .|. shiftMask, xK_Escape)
              , confirmPrompt
                prompt
                "that, 'I, [insert name here], [DATA EXPUNGED] ...lol!'" $
                io exitSuccess -- Quits xmonad.
              )
            ] ++

            -- | Deez Nuts:
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

          myMouseBindings XConfig {..} =
            M.fromList
            [ ( (modMask, button1)
              , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
              ) -- mod, mouse-1: Sets window to floating mode; move by dragging.

            , ((modMask, button2), \w -> focus w >> windows W.shiftMaster) -- mod, mouse-2: Raises window to top of stack.
            , ((modMask, button3), Flex.mouseWindow Flex.discrete) ---------- mod, mouse-3: Optional; (subjectively) ostensibly "nicer" than so-called "normal" mouse movement and/or resizing, at least in theory.

            , ( (modMask .|. altMask, button3)
              , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
              ) -- mod+alt, mouse-3: Sets window to floating mode aggressively; resize aggressively by dragging.

            , ((modMask, button4), const $ windows W.swapDown)
            , ((modMask, button5), const $ windows W.swapUp)
            ]

          -- | KeyMask
          --------------------------------------------------------------------------------
          --------------------------------------------------------------------------------
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

          myLayoutHook =
            mkToggle (single REFLECTX) $
            mkToggle (single REFLECTY) $
            avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
            where
              myDefaultLayout = refocusLastLayoutHook . trackFloating $ layoutSelect
                where
                  layoutSelect =
                    tiled |||
                    Mirror tiled |||
                    TwoPane (3 / 100) (1 / 2) |||
                    ThreeColMid 1 (3 / 100) (1 / 2) |||
                    simplestFloat ||| Roledex ||| StackTile 1 (3 / 100) (1 / 2)
                    where
                      tiled = Tall nmaster delta ratio -- Default tiling algorithm; partitions the screen into two panes.
                      nmaster = 1 ----------------------- The default number of windows in the master pane.
                      ratio = 1 / 2 --------------------- Default proportion of screen occupied by master pane.
                      delta = 3 / 100 ------------------- Percentage of screen by which to increment when resizing panes.

          -- | X ()
          --------------------------------------------------------------------------------
          --------------------------------------------------------------------------------
          scratchHtop = namedScratchpadAction myScratchPads "monitor"
          scratchAlsamixer = namedScratchpadAction myScratchPads "alsamixer"
          up = updatePointer (0.5, 0.5) (0, 0)
          warpToCenter =
            gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x 0.5 0.5

          --------------------------------------------------------------------------------
          -- | Named Scratchpads
          --------------------------------------------------------------------------------
          myScratchPads :: [NamedScratchpad]
          myScratchPads =
            [ NS "htop" spawnHtop findHtop manageHtop
            , NS "alsamixer" spawnAlsamixer findAlsamixer manageAlsamixer
            ]
            where
              spawnHtop = myOtherTerminal ++ " --title htop -e htop"
              findHtop = title =? "htop"
              manageHtop = customFloating $ W.RationalRect l t w h
                where
                  h = 0.9
                  w = 0.9
                  t = 0.95 - h
                  l = 0.95 - w
          spawnAlsamixer = myOtherTerminal ++ " --title alsamixer -e alsamixer"
          findAlsamixer = title =? "alsamixer"
          manageAlsamixer = customFloating $ W.RationalRect l t w h
            where
              h = 0.5
              w = 0.4
              t = 0.75 - h
              l = 0.70 - w

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
          mygridConfig =
            (buildDefaultGSConfig myColorizer)
              { gs_cellheight = 64
              , gs_cellwidth = 256
              , gs_cellpadding = 5
              , gs_originFractX = 0.5
              , gs_originFractY = 0.5
              , gs_font = myFontMeh
              }
          myColorizer =
            colorRangeFromClassName
              (0x22, 0x22, 0x22) -- Lowest inactive BG
              (0xdd, 0xdd, 0xdd) -- Highest inactive BG
              (0x00, 0x00, 0x00) -- Active BG
              (0x00, 0x00, 0x00) -- Inactive FG
              (0xff, 0x00, 0xff) -- Active FG
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
                      [ ((controlMask, xK_w), P.killWord' isSpace Prev)
                      , ((0, xK_Left), P.moveHistory W.focusUp')
                      , ((0, xK_Right), P.moveHistory W.focusDown')
                      ]
                  , P.vimLikeXPKeymap
                  ]
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
          reddit = S.searchEngine "reddit" "https://old.reddit.com/r/"

          {-# NOINLINE myHome #-}
          myHome :: String
          myHome = unsafePerformIO $ getEnv "HOME"


          myFontMeh = "xft:B612:size=8"
          myFontHuge = "xft:B612:size=32"


          curDirToWorkspacename = do
            name <- WN.getCurrentWorkspaceName
            when (isNothing name) $ do
              dir <- io getCurrentDirectory
              when (dir /= myHome) $ do
                WN.setCurrentWorkspaceName $ last $ splitOneOf "/" dir
            where
              splitOneOf = split . dropDelims . oneOf
              doCondense Condense ls = condense' ls
                where
                  condense' [] = []
                  condense' (c@(Text _):l) = c : condense' l
                  condense' l = Delim (concatMap fromElem ds) : condense' rest
                    where
                      (ds, rest) = span isDelim l
                  doCondense _ ls = ls
              fromElem (Text as)  = as
              fromElem (Delim as) = as
              oneOf elts = defaultSplitter {delimiter = Delimiter [(`elem` elts)]}
              matchDelim (Delimiter []) xs = Just ([], xs)
              matchDelim (Delimiter _) [] = Nothing
              matchDelim (Delimiter (p:ps)) (x:xs)
                | p x = matchDelim (Delimiter ps) xs >>= \(h, t) -> Just (x : h, t)
                | otherwise = Nothing
              breakDelim (Delimiter []) xs = ([], Just ([], xs))
              defaultSplitter =
                Splitter
                  { delimiter = Delimiter [const False]
                  , delimPolicy = Keep
                  , condensePolicy = KeepBlankFields
                  , initBlankPolicy = KeepBlank
                  , finalBlankPolicy = KeepBlank
                  }
              isDelim (Delim _) = True
              isDelim _         = False
              doDrop _ l = l
              postProcess s =
                dropInitial (initBlankPolicy s) .
                doMerge (delimPolicy s) .
                doDrop (delimPolicy s) .
                insertBlanks (condensePolicy s) . doCondense (condensePolicy s)
              insertBlanks _ []               = [Text []]
              insertBlanks cp (d@(Delim _):l) = Text [] : insertBlanks' cp (d : l)
              insertBlanks cp l               = insertBlanks' cp l
              insertBlanks' _ [] = []
              insertBlanks' cp@DropBlankFields (d1@(Delim _):d2@(Delim _):l) =
                d1 : insertBlanks' cp (d2 : l)
              insertBlanks' cp (d1@(Delim _):d2@(Delim _):l) =
                d1 : Text [] : insertBlanks' cp (d2 : l)
              insertBlanks' _ [d@(Delim _)] = [d, Text []]
              insertBlanks' cp (c:l) = c : insertBlanks' cp l
              splitInternal _ [] = []
              splitInternal d xxs
                | null xs = toSplitList match
                | otherwise = Text xs : toSplitList match
                where
                  (xs, match) = breakDelim d xxs
                  toSplitList Nothing = []
                  toSplitList (Just ([], r:rs)) = Delim [] : Text [r] : splitInternal d rs
                  toSplitList (Just (delim, rest)) = Delim delim : splitInternal d rest
              doMerge KeepLeft  = mergeLeft
              doMerge KeepRight = mergeRight
              doMerge _         = id
              mergeLeft []                     = []
              mergeLeft ((Delim d):(Text c):l) = Text (d ++ c) : mergeLeft l
              mergeLeft (c:l)                  = c : mergeLeft l
              mergeRight [] = []
              mergeRight ((Text c):l) = Text (c ++ d) : mergeRight lTail
                where
                  (d, lTail) =
                    case l of
                      Delim d':l' -> (d', l')
                      _           -> ([], l)
              mergeRight (c:l) = c : mergeRight l
              dropInitial DropBlank (Text []:l) = l
              dropInitial _ l                   = l
              split s = map fromElem . postProcess s . splitInternal (delimiter s)
              dropDelims s = s {delimPolicy = Drop}

          viewWith viewer tag = do
            itag <- curTag
            when' (tag /= itag) $ do
              modifyWindowSet' (viewer tag)
              Any . (tag ==) <$> curTag

                where -- | A 'when' that accepts a monoidal return value:

                  when' b ma =
                    if b
                    then ma
                    else return mempty

                  curTag = W.tag <$> curWorkspace -- | Get the current tag.

                  curWorkspace = W.workspace <$> curScreen -- | Get the current workspace.

                  curScreen = withWindowSet' (return . W.current) -- | Get the current screen.

                  withWindowSet' = (=<< gets windowset) -- | A generalisation of 'withWindowSet'.

                  modifyWindowSet' f = modify $ \xs -> xs {windowset = f (windowset xs)} -- | A generalisation of 'modifyWindowSet'.

          copyCat n s
            | Just w <- W.peek s = copyWindow w n s
            | otherwise = s

          -- colorAmber :: String = "#ffff00" -- Amber; inverse of Blue
          -- colorCyan :: String = "#00ffff" -- Cyan; inverse of Red

          colorBlue    :: String = "#0000ff" -- Blue; inverse of Amber
          colorGreen   :: String = "#00ff00" -- Green; inverse of Magenta
          colorIntOr   :: String = "#BA160C" -- International Orange
          colorMagenta :: String = "#ff00ff" -- Magenta; inverse of Green
          colorOrInt   :: String = "#45e9f3" -- Inverse of International Orange
          colorRed     :: String = "#ff0000" -- Red; inverse of Cyan

          colorBlack   :: String = "#000000" -- BLACK
          colorGray    :: String = "#7fffff" -- GRAY
          colorWhite   :: String = "#ffffff" -- WHITE

          tHSK :: Topic = "<fn=1>\xf120</fn>"
          tWEB :: Topic = "1: Browser"

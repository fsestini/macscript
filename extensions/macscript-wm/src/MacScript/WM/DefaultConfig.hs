module MacScript.WM.DefaultConfig where

import MacScript
import MacScript.WM.Core
import MacScript.WM.Operations
import MacScript.WM.Layout

import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty(..))
import System.Process
import Lens.Micro
import Control.Concurrent (forkIO, threadDelay)

altKB :: Char -> Hotkey
altKB c = Hotkey [(Alt, LeftPos)] (KeyLiteral c)

altShiftKB :: Char -> Hotkey
altShiftKB c = Hotkey [(Alt, LeftPos), (Shift, LeftPos)] (KeyLiteral c)

altCtrl :: Char -> Hotkey
altCtrl c = Hotkey [(Alt, LeftPos), (Ctrl, LeftPos)] (KeyLiteral c)

laltRshift :: Char -> Hotkey
laltRshift c = Hotkey [(Alt, LeftPos), (Shift, RightPos)] (KeyLiteral c)

infix 4 ~~>
(~~>) :: Hotkey -> m () -> (Hotkey, m ())
(~~>) = (,)

defaultKeyBindings :: [(Hotkey, WM ())]
defaultKeyBindings = second (justLog "") <$>
  [ altShiftKB 'i' ~~> sendFocusedToSpaceLeft
  , altShiftKB 'o' ~~> sendFocusedToSpaceRight

  , altKB 'x'      ~~> lift (setKeySet NormalKeySet)
  , altKB 'c'      ~~> lift (setKeySet MouseKeySet)
  , altKB 'z'      ~~> lift (setKeySet WindowKeySet)

  , altKB 'w'      ~~> (focusedWindow >>= maybe (pure ()) closeWindow)
  , altKB 'r'      ~~> msgToActive RotateClockwise
  , altKB 'n'      ~~> msgToActive DecrSplit
  , altKB 'm'      ~~> msgToActive IncrSplit
  , altShiftKB '=' ~~> msgToActive EqualizeSplit
  , altShiftKB '1' ~~> sendToDisplay 0
  , altShiftKB '2' ~~> sendToDisplay 1
  , altKB '1'      ~~> focusDisplay 0
  , altKB '2'      ~~> focusDisplay 1
  , altKB ','      ~~> msgToActive ExpandMaster
  , altKB '.'      ~~> msgToActive ShrinkMaster
  , altKB 't'      ~~> (focusedWindow >>= maybe (pure ()) tile)
  , altKB 'u'      ~~> (focusedWindow >>= maybe (pure ()) untile)

  , laltRshift 'e' ~~>
      liftIO (createProcess (proc "open" ["/Applications/Emacs.app"]) >> pure ())

  , Hotkey [(Alt, LeftPos)] KeySpace  ~~> lift nextLayout
  , Hotkey [(Alt, LeftPos)] KeyReturn ~~>
     (focusedWindow >>= maybe (pure ()) (msgToActive . SwapMaster))
  ]

normalKeyBindings :: [(Hotkey, WM ())]
normalKeyBindings = second (justLog "") <$>
  [ altKB 'j'      ~~> sendFocusMsg DirDown
  , altKB 'k'      ~~> sendFocusMsg DirUp
  , altKB 'h'      ~~> sendFocusMsg DirLeft
  , altKB 'l'      ~~> sendFocusMsg DirRight

  , altShiftKB 'j' ~~> sendSwapMsg DirDown
  , altShiftKB 'k' ~~> sendSwapMsg DirUp
  , altShiftKB 'h' ~~> sendSwapMsg DirLeft
  , altShiftKB 'l' ~~> sendSwapMsg DirRight
  , altKB ';' ~~> liftIO (do
      p <- center <$> (activeDisplay >>= displayFullFrame)
      mouseSetPosition p)
  ]
  where sendFocusMsg d =
          focusedWindow >>= maybe (pure ()) (\w -> msgToActive (FocusTowards w d))
        sendSwapMsg d =
          focusedWindow >>= maybe (pure ()) (\w -> msgToActive (SwapTowards w d))

mouseKeyBindings :: [(Hotkey, WM ())]
mouseKeyBindings = second (justLog "") <$>
  [ altKB 'j' ~~> modifyMousePos yL (+mouseDiff*3)
  , altKB 'k' ~~> modifyMousePos yL (\y -> y-mouseDiff*3)
  , altKB 'l' ~~> modifyMousePos xL (+mouseDiff*3)
  , altKB 'h' ~~> modifyMousePos xL (\x -> x-mouseDiff*3)

  , altShiftKB 'j' ~~> modifyMousePos yL (+mouseDiff)
  , altShiftKB 'k' ~~> modifyMousePos yL (\y -> y-mouseDiff)
  , altShiftKB 'l' ~~> modifyMousePos xL (+mouseDiff)
  , altShiftKB 'h' ~~> modifyMousePos xL (\x -> x-mouseDiff)

  , altKB ';' ~~> liftIO (do
      p <- mousePosition
      _ <- forkIO (threadDelay 1000000 >> mouseLeftButtonClick p)
      pure ())
  ]
  where modifyMousePos l f = mousePosition >>= mouseSetPosition . over l f
        mouseDiff = 20

windowMotionBindings :: [(Hotkey, WM ())]
windowMotionBindings = second (justLog "") <$>
  [ altKB 'j' ~~> modifyWinPos yL (+winDiff*3)
  , altKB 'k' ~~> modifyWinPos yL (\y -> y-winDiff*3)
  , altKB 'l' ~~> modifyWinPos xL (+winDiff*3)
  , altKB 'h' ~~> modifyWinPos xL (\x -> x-winDiff*3)

  , altShiftKB 'j' ~~> modifyWinPos yL (+winDiff)
  , altShiftKB 'k' ~~> modifyWinPos yL (\y -> y-winDiff)
  , altShiftKB 'l' ~~> modifyWinPos xL (+winDiff)
  , altShiftKB 'h' ~~> modifyWinPos xL (\x -> x-winDiff)

  ]
  where modifyWinPos l f =
          focusedWindow >>= maybe (pure ())
            (\w -> windowRect w >>= setWindowRect w . over originL (over l f))
        winDiff = 20

defaultConfig :: UserConfig
defaultConfig =
  UserConfig
    { _wmcTileMode    = OptIn ["Terminal", "Emacs"]
    , _wmcPadding     = 0
    , _wmcLayouts     = ("Tall", SL defaultTall) :| [("Max", SL Maximized)]
    , _wmcKeybindings = defaultKeyBindings
    , _wmcKeySets     = [ (SomeKeySet NormalKeySet, normalKeyBindings)
                        , (SomeKeySet MouseKeySet, mouseKeyBindings)
                        , (SomeKeySet WindowKeySet, windowMotionBindings)
                        ]
    }

module MacScript.Mouse
  ( MouseButton(..)
  , MouseButtonPress(..)
  , pressedMouseButton
  , mouseLeftButtonClick
  , mousePosition
  , mouseSetPosition
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe
import Control.Monad (void)
import MacScript.Error

import MacSdk
import MacSdk.Framework.AppKit

-- | Simulate a left mouse button click at the specified point.
mouseLeftButtonClick :: MonadIO m => Point -> m ()
mouseLeftButtonClick p = liftIO . void . runMaybeT $ do
  ev <- MaybeT $ createMouseEvent Nothing EventLeftMouseDown p LeftButtonPress
  liftIO $ do
    eventPost HIDEventTap ev
    eventSetType ev EventLeftMouseUp
    eventPost HIDEventTap ev

mousePosition :: MonadIO m => m Point
mousePosition = mouseLocation

mouseSetPosition :: MonadIO m => Point -> m ()
mouseSetPosition = wrapCGErr . warpMouseCursorPosition

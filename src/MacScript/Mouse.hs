module MacScript.Mouse
  ( MouseButton(..)
  -- , MouseButtonPress(..)
  , mouseCurrentButton
  , mouseLeftButtonClick
  , mousePosition
  , mouseSetPosition
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe
import Control.Monad (void)
import MacScript.Internal.Error (wrapCGErr)

import MacSdk (Point, EventType(..), MouseButtonPress(..), EventTapLocation(..),
               warpMouseCursorPosition, createMouseEvent, eventPost,
               eventSetType)
import MacSdk.Framework.AppKit (MouseButton(..), pressedMouseButton,
                                mouseLocation)

-- | Simulates a left mouse button click at the specified point.
mouseLeftButtonClick :: MonadIO m => Point -> m ()
mouseLeftButtonClick p = liftIO . void . runMaybeT $ do
  ev <- MaybeT $ createMouseEvent Nothing EventLeftMouseDown p LeftButtonPress
  liftIO $ do
    eventPost HIDEventTap ev
    eventSetType ev EventLeftMouseUp
    eventPost HIDEventTap ev

-- | Returns the current mouse position in Quartz coordinates.
mousePosition :: MonadIO m => m Point
mousePosition = mouseLocation

-- | Sets the position of the mouse cursor.
mouseSetPosition :: MonadIO m => Point -> m ()
mouseSetPosition = wrapCGErr . warpMouseCursorPosition

-- | Returns the currently depressed mouse button.
mouseCurrentButton :: MonadIO m => m MouseButton
mouseCurrentButton = pressedMouseButton

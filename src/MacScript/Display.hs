-- {-# LANGUAGE FlexibleContexts #-}

-- | MacOS display management utilities.

module MacScript.Display
  ( DisplayID
  , displays
  , MacScript.Display.activeDisplay
  , primaryDisplay
  , displayFullFrame
  , displayFrame
  , activeSpaceForDisplay
  -- * Events
  , displayAddedEvent
  , displayRemovedEvent
  , displayMovedEvent
  , displayResizedEvent
  ) where

import MacSdk
       (DisplayID, DisplayChange(..), Rect, displayCount, activeDisplay,
        activeDisplays, setDisplayCallback, removeDisplayCallback)
import MacSdk.Framework.AppKit.Screen
import MacScript.Event
import MacScript.Error
import MacScript.Internal.Display (activeSpaceIDForDisplay)
import MacScript.Internal.Space
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Extra (findM)

-- | Returns the ID of the display that currently has user input focus.
activeDisplay :: MonadIO m => m DisplayID
activeDisplay = liftIO MacSdk.activeDisplay

-- | List the 'DisplayID' of all known displays.
displays :: MonadIO m => m [DisplayID]
displays = wrapCGErr (displayCount >>= activeDisplays)

-- | Returns the ID of the primary display.
primaryDisplay :: MonadIO m => m DisplayID
primaryDisplay = mainScreen >>= screenDisplayID

-- | Returns the absolute position and size of the display, without dock and
-- menubar.
displayFrame :: MonadIO m => DisplayID -> m Rect
displayFrame did = do
  mrect <- screens >>= findM (fmap (did ==) . screenDisplayID)
  maybe (error "display-screen inconsistency") frame mrect

-- | Returns the absolute position and size of the display, including dock and
-- menubar.
displayFullFrame :: MonadIO m => DisplayID -> m Rect
displayFullFrame did = do
  mrect <- screens >>= findM (fmap (did ==) . screenDisplayID)
  maybe (error "display-screen inconsistency") fullFrame mrect

--------------------------------------------------------------------------------
-- Events

-- | Returns an event that fires whenever a display has been added.
displayAddedEvent :: Event DisplayID
displayAddedEvent = displayE DisplayAdd

-- | Returns an event that fires whenever a display has been removed.
displayRemovedEvent :: Event DisplayID
displayRemovedEvent = displayE DisplayRemove

-- | Returns an event that fires whenever a display has been moved.
displayMovedEvent :: Event DisplayID
displayMovedEvent = displayE DisplayMoved

-- | Returns an event that fires whenever a display has been resized.
displayResizedEvent :: Event DisplayID
displayResizedEvent = displayE DisplayDesktopShapeChanged

displayE :: DisplayChange -> Event DisplayID
displayE notif = Event $ \h -> do
  tok <- wrapCGErr . setDisplayCallback $ \did n ->
    if notif == n then h (fromIntegral did) else pure ()
  pure (Subscription (wrapCGErr $ removeDisplayCallback tok))

-- | Return the 'SpaceID' of the space that is currently active is the dispaly
-- with given 'DisplayID'.
activeSpaceForDisplay :: MonadIO m => DisplayID -> m Space
activeSpaceForDisplay = liftIO . (>>= createSpace) . activeSpaceIDForDisplay

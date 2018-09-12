{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | Keyboard management types and functions.

module MacScript.Keyboard
  ( ModifierPress
  , Modifier(..)
  , ModifierPos(..)
  , Key(..)
  , Hotkey(..)
  , (<+>)
  , lalt
  , lshift
  , key
  
  -- * Events
  , keyDownEvent
  , keyUpEvent
  ) where

import MacSdk hiding (Event)
import MacSdk.Framework.Keyboard

import Control.Monad.IO.Class (MonadIO(..))
import MacScript.Event
import Data.Set (fromList)
import Data.List (find)

-- | Type of simple key combinations. These are the simplest key combinations
-- that can be subscribed for keypress events.
data Hotkey = Hotkey
  { _hkModifiers :: [ModifierPress]
  -- ^ Zero or more modifier keys that are associated to the hotkey.
  , _hkKey :: Key
  -- ^ The single non-modifier key that is associated to the hotkey.
  }
  deriving (Show, Eq)

infix 5 <+>
(<+>) :: [ModifierPress] -> Key -> Hotkey
(<+>) = Hotkey

lalt :: [ModifierPress]
lalt = [(Alt, LeftPos)]

lshift :: [ModifierPress]
lshift = [(Shift, LeftPos)]

key :: Char -> Key
key = KeyLiteral

toEvent :: KeyEventPosition -> EventType
toEvent KeyDown = EventKeyDown
toEvent KeyUp = EventKeyUp

keyCallback
  :: KeyEventPosition -> (Hotkey -> IO ()) -> [(KeyPress, Hotkey)] -> EventTapCallback ()
keyCallback ev h kp _ ety e _ = case ev of
  KeyDown -> case ety of { EventKeyDown -> go ; _ -> pure (Just e) }
  KeyUp -> case ety of { EventKeyUp -> go ; _ -> pure (Just e) }
  where
    go = do
      flags <- eventFlags e
      keycode <- getEventField e KeyboardEventKeycode
      let press = KPress (fromIntegral keycode) (fromList (toModPress (fromIntegral flags)))
      maybe (pure (Just e)) ((>> pure Nothing) . h . snd) (find ((== press) . fst) kp)

toKeyPress :: Hotkey -> KeyPress
toKeyPress (Hotkey mps kk) = KPress (toKeycode kk) (fromList mps)

keyEvent'
  :: MonadIO m => KeyEventPosition -> [Hotkey] -> (Hotkey -> IO ()) -> m EventTap
keyEvent' kpos hks k =
  (liftIO $ eventTapCreate kCGSessionEventTap kCGHeadInsertEventTap
    kCGEventTapOptionDefault (EventList [toEvent kpos])
      (keyCallback kpos k (zip (fmap toKeyPress hks) hks)) ()) >>= \case
    Just etap ->
      liftIO (addSourceToMainLoop (eventTapMachPort etap) >> pure etap)
    Nothing -> error "unspecified error when trying to establish keyboard event tap"

keyEvent :: KeyEventPosition -> [Hotkey] -> Event Hotkey
keyEvent ev kp = Event $ fmap (Subscription . eventTapRelease) . keyEvent' ev kp

-- | Source of events that fire when one of the specified hotkeys are
-- down-pressed. Event connection can fail with unspecified errors.
keyDownEvent :: [Hotkey] -> Event Hotkey
keyDownEvent = keyEvent KeyDown

-- | Source of events that fire when one of the specified hotkeys are
-- up-pressed. Event connection can fail with unspecified errors.
keyUpEvent :: [Hotkey] -> Event Hotkey
keyUpEvent = keyEvent KeyUp

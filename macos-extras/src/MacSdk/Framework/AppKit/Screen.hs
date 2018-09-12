module MacSdk.Framework.AppKit.Screen
  ( Screen
  , screenDisplayID
  , mainScreen
  , screens
  , fullFrame
  , frame
  , primaryScreen
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign
import MacSdk.Framework.CoreFoundation
import MacSdk.Framework.CoreGraphics.Rect
import MacSdk.Framework.CoreGraphics.Display

data NSScreen
newtype Screen = Screen { unScreen :: Ptr NSScreen }

foreign import ccall unsafe screens_ref :: IO CFArrayRef
foreign import ccall unsafe main_screen :: IO (Ptr NSScreen)
foreign import ccall unsafe screen_full_frame :: Ptr NSScreen -> Ptr Rect -> IO ()
foreign import ccall unsafe screen_frame :: Ptr NSScreen -> Ptr Rect -> IO ()
foreign import ccall unsafe screen_id :: Ptr NSScreen -> IO DisplayID

-- | Retrieves the 'DisplayID' value associated with the screen.
screenDisplayID :: MonadIO m => Screen -> m DisplayID
screenDisplayID = liftIO . screen_id . unScreen

-- | Returns the screen object containing the window with the keyboard focus.
mainScreen :: MonadIO m => m Screen
mainScreen = liftIO $ Screen <$> main_screen

-- | Returns a list of all the screens available on the system.
screens :: MonadIO m => m [Screen]
screens = liftIO $ fmap Screen <$> (screens_ref >>= fmap (fmap castPtr) . getCFArrayValues)

primaryScreen :: MonadIO m => m Screen
primaryScreen = fmap (!! 0) screens

-- | The dimensions and location of the screen, including dock and menubar.
fullFrame :: MonadIO m => Screen -> m Rect
fullFrame (Screen nss) = liftIO $ alloca $ \r -> screen_full_frame nss r >> peek r

-- | The dimensions and location of the screen, without dock and menubar.
frame :: MonadIO m => Screen -> m Rect
frame (Screen nss) = liftIO $ alloca $ \r -> screen_frame nss r >> peek r

{-# LANGUAGE LambdaCase #-}

module MacSdk.Framework.AppKit.Event where

import MacSdk.Framework.CoreGraphics.Rect
import MacSdk.Framework.AppKit.Screen
import Foreign
import Data.Word
import Control.Monad.IO.Class (MonadIO(..))
import Lens.Micro

foreign import ccall unsafe
  mouse_location :: Ptr Point -> IO ()

-- | Returns the current mouse position in Quartz coordinates.
mouseLocation :: MonadIO m => m Point
mouseLocation = do
  p <- liftIO (alloca $ \p -> mouse_location p >> peek p)
  prim <- primaryScreen >>= fullFrame
  let primMaxY = prim^.originL.yL + prim^.sizeL.heightL
  pure $ over yL (\y -> primMaxY - y) p

foreign import ccall unsafe
  pressed_mouse_buttons :: IO Word64

data MouseButton
  = NoButton
  | LeftButton
  | RightButton
  | OtherButton
  deriving (Show)

-- | Returns the currently depressed mouse button.
pressedMouseButton :: MonadIO m => m MouseButton
pressedMouseButton = liftIO $ pressed_mouse_buttons >>= pure . \case
  0 -> NoButton
  1 -> LeftButton
  2 -> RightButton
  _ -> OtherButton

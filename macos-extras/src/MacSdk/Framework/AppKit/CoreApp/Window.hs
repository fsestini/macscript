module MacSdk.Framework.AppKit.CoreApp.Window where

import MacSdk.Framework.CoreGraphics.Rect
import MacSdk.Framework.NSObject
import Foreign
import Foreign.C.Types (CBool(..))
import Data.Word
import Data.Bits
import Control.Monad.IO.Class (MonadIO(..))

type NSWindowStyleMask = Word32
type NSBackingStoreBuffered = Word32

data WindowStyleMask
  = Borderless
  | Titled
  | Closable
  | Miniaturizable
  | Resizable

foreign import ccall unsafe maskBorderless :: NSWindowStyleMask
foreign import ccall unsafe maskTitled :: NSWindowStyleMask
foreign import ccall unsafe maskClosable :: NSWindowStyleMask
foreign import ccall unsafe maskMiniaturizable :: NSWindowStyleMask
foreign import ccall unsafe maskResizable :: NSWindowStyleMask

toNSMask :: WindowStyleMask -> NSWindowStyleMask
toNSMask = \case
  Borderless -> maskBorderless
  Titled -> maskTitled
  Closable -> maskClosable
  Miniaturizable -> maskMiniaturizable
  Resizable -> maskResizable

data DeferWindow = Yes | No
toDeferBool :: DeferWindow -> CBool
toDeferBool Yes = 1
toDeferBool No = 0

data NSWindow
instance NSClass NSWindow
newtype Window = Window { getWindow :: ForeignPtr NSWindow }
  deriving (NSObject)

foreign import ccall unsafe windowAllocInitWithContentRect_
  :: Ptr Rect -> NSWindowStyleMask -> CBool -> IO (Ptr NSWindow)

windowNewFromRect
  :: MonadIO m
  => Rect -> [WindowStyleMask] -> DeferWindow -> m Window
windowNewFromRect r msks def = liftIO $ alloca $ \p -> do
  poke p r
  w <- windowAllocInitWithContentRect_ p
         (foldr (.|.) 0 (fmap toNSMask msks)) (toDeferBool def)
  manageNSPtr w

foreign import ccall unsafe windowMakeKeyAndOrderFront_ :: Ptr NSWindow -> IO ()

windowMakeKeyAndOrderFront :: MonadIO m => Window -> m ()
windowMakeKeyAndOrderFront = liftIO . flip withNSPtr windowMakeKeyAndOrderFront_

foreign import ccall unsafe windowClose_ :: Ptr NSWindow -> IO ()

windowClose :: MonadIO m => Window -> m ()
windowClose = liftIO . flip withNSPtr windowClose_

foreign import ccall unsafe setReleasedWhenClosed_ :: Ptr NSWindow -> CBool -> IO ()

setReleasedWhenClosed :: MonadIO m => Window -> Bool -> m ()
setReleasedWhenClosed w b =
  liftIO (withNSPtr w (flip setReleasedWhenClosed_ (fromBool b)))

module MacScript.MenuBar (StatusItem, newStatusItem, setStatusItemTitle) where

import MacSdk
import Foreign hiding (with)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed

data NSStatusBar
data NSStatusItem

newtype StatusItem = StatusItem { getItem :: ForeignPtr NSStatusItem }

foreign import ccall unsafe systemStatusBar_ :: IO (Ptr NSStatusBar)
foreign import ccall unsafe newItem :: Ptr NSStatusBar -> IO (Ptr NSStatusItem)
foreign import ccall unsafe
  setItemTitle :: Ptr NSStatusItem -> CFStringRef -> IO ()
foreign import ccall unsafe retainItem :: Ptr NSStatusItem -> IO ()
foreign import ccall unsafe "&releaseItem"
  releaseItemPtr :: FunPtr (Ptr NSStatusItem -> IO ())

newStatusItem :: MonadIO m => m StatusItem
newStatusItem = liftIO $ do
  p <- systemStatusBar_ >>= newItem
  retainItem p
  fp <- newForeignPtr releaseItemPtr p
  pure (StatusItem fp)

setStatusItemTitle :: MonadIO m => StatusItem -> String -> m ()
setStatusItemTitle (StatusItem fp) str = liftIO $ do
  cfstr <- fromString CFStringEncodingASCII str
  flip with pure $ do
    strptr <- managed (withCFPtr cfstr)
    p <- managed (withForeignPtr fp)
    liftIO (setItemTitle p strptr)

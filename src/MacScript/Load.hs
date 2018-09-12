module MacScript.Load
  ( MonadMac, Mac, runScript, stopScript
  ) where

import MacSdk
import MacSdk.Framework.AppKit
import MacSdk.Framework.Keyboard

import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift

class MonadIO m => MonadMac m where
  askMacConfig :: m MacConfig

instance MonadMac Mac where
  askMacConfig = Mac ask

newtype MacConfig = MacConfig { _mcfgRunLoop :: RunLoop }
newtype Mac a = Mac { unMac :: ReaderT MacConfig IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

runScript :: Mac () -> IO ()
runScript body = (>>= either (putStrLn . ("error: " ++)) pure) . runExceptT $ do
  err "failed to init keycode map" initKeycodeMap
  err "the process does not have Accessibility privileges" checkAXPrivileges
  err "error during Cocoa initialization" applicationLoad

  liftIO $ do
    rl <- runLoopGetCurrent
    runReaderT (unMac body) (MacConfig rl)
    runLoopRun

  where
    err str m = ExceptT $ fmap (\b -> if b then Right () else Left str) m

stopScript :: MonadMac m => m ()
stopScript = askMacConfig >>= liftIO . runLoopStop . _mcfgRunLoop

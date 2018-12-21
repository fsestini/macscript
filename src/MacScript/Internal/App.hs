module MacScript.Internal.App where

import Data.Traversable (for)

import MacSdk (UIElement, AXError, AsAXError, createAppElement,
               copyAttributeNames, toString, CFStringEncoding(..), arrayValues)
import MacSdk.Framework.Accessibility.Attribute.Types
  (SomeAttribute(..), Attribute(..), toAttributeString)

import MacScript.Prelude
import MacScript.Process (processName)
import MacScript.Error (retryOnCannotComplete)
import MacScript.Internal.Process (CarbonProcess(..))

data App = App
  { _appProcess :: CarbonProcess
  , _appElement :: UIElement
  , _appName :: String
  }

instance Show App where
  show (App p _ name) = "App " ++ show (_crbnPID p) ++ " " ++ name

mkAppRetry :: MonadIO m => Int -> CarbonProcess -> ExceptT AXError m (Maybe App)
mkAppRetry n p =
  retryOnCannotComplete n $ do
    el <- createAppElement (_crbnPID p)
    b <- supportsAttributes el
      [ SomeAttribute WindowsAttribute
      , SomeAttribute FocusedWindowAttribute
      ]
    if b then Just . App p el <$> processName p else pure Nothing

supportsAttributes
  :: (MonadIO m, MonadError e m, AsAXError e) => UIElement -> [SomeAttribute] -> m Bool
supportsAttributes el attrs = do
  arr <- copyAttributeNames el
  list <- join .
    fmap (fmap catMaybes .
          traverse (toString CFStringEncodingASCII)) . arrayValues $ arr
  liftIO . fmap and $ traverse (supportsAttribute list) attrs
  where supportsAttribute list (SomeAttribute attr) =
          do
            str <- toAttributeString attr
            res <- fmap or . for list $ \str' -> do
              let b = str == str'
              pure b
            pure res

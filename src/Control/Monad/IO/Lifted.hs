-- | Common IO operations lifted to any 'MonadIO' instance.

module Control.Monad.IO.Lifted
  ( module Control.Monad.IO.Class
  , putStr'
  , putStrLn'
  , print'
  ) where

import Control.Monad.IO.Class

putStr' :: MonadIO m => String -> m ()
putStr' = liftIO . putStr

putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

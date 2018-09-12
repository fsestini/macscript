module MacScript.Prelude
  ( module Control.Monad.Catch
  , module Control.Monad.Except
  , module Control.Monad.Trans.Maybe
  , module Control.Monad.IO.Class
  , module Data.Maybe
  , module Control.Monad
  , module Control.Concurrent
  , headMay
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

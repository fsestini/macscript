-- | 

module MacSdk.Framework.Foundation.OperationQueue where

import Foreign
import Control.Monad.IO.Class

data NSOperationQueue
newtype OperationQueue = OperationQueue
  { getOperationQueue :: Ptr NSOperationQueue
  }

foreign import ccall unsafe main_queue :: IO (Ptr NSOperationQueue)

mainQueue :: MonadIO m => m OperationQueue
mainQueue = liftIO (fmap OperationQueue main_queue)

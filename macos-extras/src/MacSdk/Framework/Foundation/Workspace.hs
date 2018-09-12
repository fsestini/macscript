-- | 

module MacSdk.Framework.Foundation.Workspace where

import Foreign
import Control.Monad.IO.Class

data NSWorkspace
newtype Workspace = Workspace { getWorkspace :: Ptr NSWorkspace }

foreign import ccall unsafe shared_workspace :: IO (Ptr NSWorkspace)

sharedWorkspace :: MonadIO m => m Workspace
sharedWorkspace = liftIO (fmap Workspace shared_workspace)

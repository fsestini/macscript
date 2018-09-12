{-# LANGUAGE MultiWayIf #-}

-- | MacOS process management types and functions.

module MacScript.Process
  ( CarbonProcess
  , PSN
  , PID
  , processName
  , interactiveProcs
  , processPID
  , setFrontProcessFrontWindowOnly
  , focusedProcess
  , isProcessHidden
  , carbonProcessPID
  ) where

import MacSdk
import MacScript.AppTypes
import Control.Monad.IO.Class (MonadIO(..))
import MacSdk.Framework.Carbon

-- | Returns whether the application with given process is hidden.
-- It returns @Nothing@ if no such application exists.
isProcessHidden :: MonadIO m => CarbonProcess -> m (Maybe Bool)
isProcessHidden = liftIO . isProcHidden . _crbnPID

-- | Returns whether the process is interactive.
interactiveCarbonProc :: CarbonProcess -> Bool
interactiveCarbonProc CProc{..} = isProcessInteractive _crbnBackground _crbnPolicy

-- | Returns all processes that are interactive, i.e. non-background and with a
-- regular policy.
interactiveProcs :: MonadIO m => m [CarbonProcess]
interactiveProcs =
  liftIO $ filter interactiveCarbonProc <$> (allPSNs >>= mapM carbonProcess)

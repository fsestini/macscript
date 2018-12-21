module MacScript.Internal.Process where

import MacSdk (PID)
import MacSdk.Framework.Carbon (PSN, ProcPolicy, processPID, processPolicy,
                                getIsBackground)

data CarbonProcess = CProc
  { _crbnPID :: PID
  , _crbnPSN :: PSN
  , _crbnPolicy :: ProcPolicy
  , _crbnBackground :: Bool
  } deriving (Show)

carbonProcessPID :: CarbonProcess -> PID
carbonProcessPID = _crbnPID

carbonProcessPSN :: CarbonProcess -> PSN
carbonProcessPSN = _crbnPSN

-- | Retrieve the Carbon process associated to a Process Serial Number.
carbonProcess :: PSN -> IO CarbonProcess
carbonProcess psn =
  CProc <$> processPID psn
        <*> pure psn
        <*> processPolicy psn
        <*> getIsBackground psn

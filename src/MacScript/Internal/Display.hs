module MacScript.Internal.Display where

import Control.Monad ((>=>))
import MacSdk (DisplayID, CFString, SpaceID, displayUUID, uuidString', currentSpace')

displayUUIDString :: DisplayID -> IO CFString
displayUUIDString = displayUUID >=> uuidString'

activeSpaceIDForDisplay :: DisplayID -> IO SpaceID
activeSpaceIDForDisplay = displayUUIDString >=> currentSpace'

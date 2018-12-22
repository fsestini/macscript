module MacScript.Internal.Space where

import MacSdk (SpaceID, SpaceType, WindowID, spaceType', activeDisplay,
               displayForWindow', currentSpace')
import MacScript.Internal.Display
import Control.Monad ((>=>))
data Space = Space
  { spcID :: SpaceID
  , spcType :: SpaceType
  } deriving (Show, Eq)

activeSpaceID :: IO SpaceID
activeSpaceID = MacSdk.activeDisplay >>= activeSpaceIDForDisplay

createSpace :: SpaceID -> IO Space
createSpace sid = Space sid <$> spaceType' sid

-- | Returns the 'SpaceID' of the space to which the window with a given
-- 'WindowID' belongs to.
spaceIDForWindowID :: WindowID -> IO SpaceID
spaceIDForWindowID = displayForWindow' >=> currentSpace'

module MacScript.Internal.Space where

import MacSdk (SpaceID, SpaceType, spaceType')

data Space = Space
  { spcID :: SpaceID
  , spcType :: SpaceType
  } deriving (Show, Eq)

createSpace :: SpaceID -> IO Space
createSpace sid = Space sid <$> spaceType' sid

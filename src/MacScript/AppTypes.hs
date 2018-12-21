{-# LANGUAGE FlexibleContexts #-}

module MacScript.AppTypes where

import MacSdk

import MacScript.Prelude
import MacScript.Internal.Window (Window(..))

-- | Returns whether the two windows have the same identifier.
--
-- @
-- sameWID w1 w2 == (windowID w1 == windowID w2)
-- @
sameWID :: Window -> Window -> Bool
sameWID w1 w2 = _windowID w1 == _windowID w2

-- startObserver :: Observer -> IO ()
-- startObserver obs = do
--   mrl <- mainRunLoop
--   rls <- observerGetRunLoopSource obs
--   b <- runLoopContainsSource mrl rls DefaultMode
--   if b then pure () else runLoopAddSource mrl rls DefaultMode

data Space = Space
  { spcID :: SpaceID
  , spcType :: SpaceType
  } deriving (Show, Eq)

createSpace :: SpaceID -> IO Space
createSpace sid = Space sid <$> spaceType' sid

displayUUIDString :: DisplayID -> IO CFString
displayUUIDString = displayUUID >=> uuidString'

activeSpaceIDForDisplay :: DisplayID -> IO SpaceID
activeSpaceIDForDisplay = displayUUIDString >=> currentSpace'

activeSpaceID :: IO SpaceID
activeSpaceID = MacSdk.activeDisplay >>= activeSpaceIDForDisplay

-- | Return the 'SpaceID' of the space that is currently active is the dispaly
-- with given 'DisplayID'.
activeSpaceForDisplay :: MonadIO m => DisplayID -> m Space
activeSpaceForDisplay = liftIO . (>>= createSpace) . activeSpaceIDForDisplay

-- | Returns the 'SpaceID' of the space to which the window with a given
-- 'WindowID' belongs to.
spaceIDForWindowID :: WindowID -> IO SpaceID
spaceIDForWindowID = displayForWindow' >=> currentSpace'

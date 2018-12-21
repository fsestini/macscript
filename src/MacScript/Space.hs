{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MacOS Spaces management types and functions.

module MacScript.Space
  ( Space(..)
  , SpaceType(..)
  , SpaceID

  -- * Query
  , activeSpace
  , activeSpaceForDisplay
  , spaceHasWindow
  , allSpaces
  , displaySpaces
  , activeDisplaySpaces
  
  -- * Events
  , displayChangedEvent
  , spaceChangedEvent
  , appLaunchedEvent
  , appTerminatedEvent
  , appActivatedEvent
  , appDeactivatedEvent
  , appHiddenEvent
  , appVisibleEvent
  ) where

import MacSdk hiding (Event, activeDisplay)
import MacSdk.Framework.Foundation
import MacSdk.Framework.Carbon

import MacScript.Prelude
import MacScript.Event
import MacScript.Display
import MacScript.AppTypes
import MacScript.Error
import MacScript.Internal.Process (carbonProcess)
import MacScript.Internal.App (App, mkAppRetry)

import Data.List.NonEmpty (NonEmpty(..), nonEmpty)

nsEvent
  :: (Notification -> IO b)
  -> (b -> IO (Maybe a))
  -> NotificationName
  -> Event a
nsEvent f g name = Event $ \k -> do
  center <- sharedWorkspace >>= notificationCenter
  queue <- mainQueue
  
  obs <- addObserverForName center name nullObject queue $ \n -> do
    x <- f n
    forkIO (g x >>= maybe (pure ()) k) >> pure ()
  pure $ Subscription (removeObserver center obs)

mkAppFromPID :: PID -> IO (Maybe App)
mkAppFromPID pid = do
  psn <- processPSN pid
  prcs <- carbonProcess psn
  maybeOnInvalidOrTimeout' (mkAppRetry 5 prcs)

appLaunchedEvent :: Event App
appLaunchedEvent =
  nsEvent notificationPID mkAppFromPID
    WorkspaceDidLaunchApplicationNotification

appTerminatedEvent :: Event PID
appTerminatedEvent =
  nsEvent notificationPID (pure . pure)
    WorkspaceDidTerminateApplicationNotification

appActivatedEvent :: Event App
appActivatedEvent =
  nsEvent notificationPID mkAppFromPID
    WorkspaceDidActivateApplicationNotification

appDeactivatedEvent :: Event PID
appDeactivatedEvent =
  nsEvent notificationPID (pure . pure)
    WorkspaceDidDeactivateApplicationNotification

appHiddenEvent :: Event App
appHiddenEvent =
  nsEvent notificationPID mkAppFromPID
    WorkspaceDidHideApplicationNotification

appVisibleEvent :: Event App
appVisibleEvent =
  nsEvent notificationPID mkAppFromPID
    WorkspaceDidUnhideApplicationNotification

displayChangedEvent :: Event ()
displayChangedEvent =
  nsEvent (const (pure ())) (pure . Just)
    WorkspaceActiveDisplayDidChangeNotification

spaceChangedEvent :: Event ()
spaceChangedEvent =
  nsEvent (const (pure ())) (pure . Just)
    WorkspaceActiveSpaceDidChangeNotification

--------------------------------------------------------------------------------

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p xs = fmap (headMay . catMaybes) . sequence $
  fmap (\x -> p x >>= \b -> pure (if b then Just x else Nothing)) xs

  -- fromMaybe (error "empty list of spaces") . nonEmpty . 
allSpaces' :: IO [(CFString, NonEmpty Space)]
allSpaces' = do
  conn <- defaultConnection
  arr <- managedDisplaySpaces conn
  dicts <- arrayValues arr :: IO [Dictionary]
  forM dicts $ \dict -> do
    did <- strKey "Display Identifier" >>= getDictValue dict :: IO CFString
    sarr <- strKey "Spaces" >>= getDictValue dict
    spaceDicts <- arrayValues (Array sarr) :: IO [Dictionary]
    ss <- forM spaceDicts $ \space -> do
      num <- strKey "id64" >>= getDictValue space :: IO Number
      msid <- fmap fromIntegral <$>
        numberGetValue num NumberSigned64Type :: IO (Maybe SpaceID)
      maybe (pure Nothing) (\sid -> spaceType' sid >>= \sty ->
        pure (Just (Space sid sty))) msid
    pure (did, fromMaybe (error "empty list of spaces") . nonEmpty $ catMaybes ss)
  where strKey = fromString CFStringEncodingASCII

-- | Returns whether the space currently contains the specified window.
spaceHasWindow :: MonadIO m => Space -> Window -> m Bool
spaceHasWindow sp w = liftIO (spaceIDHasWindowID (spcID sp) (_windowID w))

spaceIDHasWindowID :: SpaceID -> WindowID -> IO Bool
spaceIDHasWindowID sid wid = do
  num <- numberCreate NumberIntType (fromIntegral wid)
  arr <- createArray' [num]
  def <- defaultConnection
  spaces <- spacesForWindows def SpaceAll arr
  spaceIds <- arrayValues spaces :: IO [Number]
  mays <- catMaybes <$>
    mapM (fmap (fmap fromIntegral) . flip numberGetValue NumberIntType) spaceIds
  pure (sid `elem` mays)

-- | Returns an association list of all displays, together with all their
-- spaces.
allSpaces :: MonadIO m => m [(DisplayID, NonEmpty Space)]
allSpaces = do
  dids <- displays
  liftIO $ do
    sps <- allSpaces'
    x <- forM dids $ \did -> do
      uuid <- displayUUIDString did
      m <- findM (objEquals uuid . fst) sps
      pure $ fmap ((did,) . snd) m
    pure . catMaybes $ x

-- | Retrieves all spaces of a given display, if it exists. This is just a
-- convenience function that perform a lookup into the structure returned by
-- 'allSpaces'.
displaySpaces :: MonadIO m => DisplayID -> m (Maybe (NonEmpty Space))
displaySpaces d = lookup d <$> allSpaces

activeDisplaySpaces :: MonadIO m => m (NonEmpty Space)
activeDisplaySpaces = fromMaybe err <$> (activeDisplay >>= displaySpaces)
  where err = error "display-space inconsistecy"

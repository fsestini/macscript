module MacScript.WM.Operations where

import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Composition
import MacScript
import MacScript.WM.Core
import Data.List.NonEmptyZipper (next, previous, fromNonEmpty, nextMod
                                ,setCurrent, _current)
import Lens.Micro (_Just, at)
import Lens.Micro.Mtl ((%=), use, (.=))
import Control.Applicative ((<|>), liftA2)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forM, filterM, join, (>=>))
import Safe (headMay, atMay)
import MacScript.MenuBar

setKeySet :: KeySet ks => ks -> WM ()
setKeySet ks = do
  wmsKeySet .= SomeKeySet ks
  item <- _wmcStatusItem <$> ask
  setStatusItemTitle item (keySetStatusItem ks)

msgToActive :: (Message msg) => msg -> EWM ()
msgToActive m = activeSpace >>= flip sendMsgTo m

sendMsgTo :: (Message msg) => Space -> msg -> EWM ()
sendMsgTo sp m = overWorkspace sp (lift .: handleMsg (SomeMessage m))

setRectAndCatch :: MonadIO m => Window -> Rect -> m ()
setRectAndCatch w r = handleUIErrM (const msg) $ setWindowRect w r
  where msg = liftIO (putStrLn "could not set frame for invalid window")

renderTree :: WM ()
renderTree = justLog "renderTree: " $ do
  m <- activeDisplay
  drect <- displayFrame m
  sp <- activeSpaceForDisplay m
  pd <- fmap (_wmcPadding . _wmcUser) ask
  lift (checkS (spcID sp))

  workspace sp >>= \(SL l, tws) ->
    lift . draw . fmap (second (pad pd)) $ render drect l tws

  where draw = mapM_ (uncurry setRectAndCatch)

-- | Switch to next layout for the currently focused space.
nextLayout :: WM ()
nextLayout = do
  sid <- fmap spcID activeSpace
  wmsSpaceLayouts . at sid . _Just %= nextMod

--------------------------------------------------------------------------------
-- Tiling operations

configAllowsTile :: Window -> WM Bool
configAllowsTile w = do
  tm <- fmap (_wmcTileMode . _wmcUser) ask
  pure $ appCanTile tm (appName (windowParent w))
  where
    appCanTile (OptIn appz) app = app `elem` appz
    appCanTile (OptOut appz) app = app `notElem` appz

tile :: Window -> EWM ()
tile w = do
  tiledIDs <- fmap twWid <$> use wmsTiled
  if windowID w `notElem` tiledIDs
    then do
      title <- fmap (fromMaybe "<unknown>") (windowTitle w)
      liftIO . putStrLn $ "Tiling: " ++ show (windowID w) ++ ":" ++ title

      tw <- TW w . Just <$> windowRect w
      sp <- windowSpace w
      sendMsgTo sp (InsertNew tw)

      lift renderTree
    else pure ()

untile :: Window -> EWM ()
untile w = do
  m <- fmap (find (sameWID w . _twWindow)) (use wmsTiled)
  case m of
    Nothing -> pure () -- window is not managed by us
    Just tw -> do
      wmsTiled %= filter (not . sameWID w . _twWindow)
      maybe (pure ()) (setRectAndCatch (_twWindow tw)) (_twFloatRect tw)
      lift renderTree

--------------------------------------------------------------------------------
-- Focusing operations

windowsInSpace :: MonadIO m => Space -> m [Window]
windowsInSpace sp = do
  apps <- interactiveApps
  winz <- concat <$> forM apps (handleUIErr (const []) . windows)
  filterM (spaceHasWindow sp) winz

focusSpace :: Space -> EWM ()
focusSpace sp = do
  mwin <- lift (liftA2 (<|>) firstTiled firstAll)
  maybe err focusWindow mwin
  where
    firstTiled = fmap join . eToM $ fmap _twWindow . headMay . snd <$> workspace sp
    firstAll = fmap join . eToM $ headMay <$> windowsInSpace sp
    err = throwError NoWindowsInSpace

spaceAtDisplay :: Int -> EWM Space
spaceAtDisplay i = do
  ds <- displays
  d <- maybe (throwError (NoDisplayIdx i)) pure (atMay ds i)
  activeSpaceForDisplay d

focusDisplay :: Int -> EWM ()
focusDisplay = spaceAtDisplay >=> focusSpace

--------------------------------------------------------------------------------
-- Moving operations

sendWindowToDisplay :: Int -> Window -> EWM ()
sendWindowToDisplay i w = do
  spaceAtDisplay i >>= flip moveToSpace w
  activeSpace >>= focusSpace

sendToDisplay :: Int -> EWM ()
sendToDisplay i =
  handleUIErr (const Nothing) focusedWindow
    >>= maybe (pure ()) (sendWindowToDisplay i)

-- TODO: ugly. tidy up
sendFocusedToSpaceLeft :: MonadIO m => m ()
sendFocusedToSpaceLeft = do
  mw <- handleUIErr (const Nothing) focusedWindow
  flip (maybe (pure ())) mw $ \w -> do
    sp <- activeSpace
    nesp <- fmap (_current . previous . fromMaybe (error "space inconsistency") .
                 setCurrent sp . fromNonEmpty) activeDisplaySpaces
    if sp == nesp then pure () else moveToSpace nesp w

sendFocusedToSpaceRight :: MonadIO m => m ()
sendFocusedToSpaceRight = do
  mw <- handleUIErr (const Nothing) focusedWindow
  flip (maybe (pure ())) mw $ \w -> do
    sp <- activeSpace
    nesp <- fmap (_current . next . fromMaybe (error "space inconsistency") .
                 setCurrent sp . fromNonEmpty) activeDisplaySpaces
    if sp == nesp then pure () else moveToSpace nesp w

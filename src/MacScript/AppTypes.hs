{-# LANGUAGE FlexibleContexts #-}

module MacScript.AppTypes where

import MacSdk
import MacSdk.Framework.Accessibility.Attribute.Types
import MacSdk.Framework.Carbon

import MacScript.Error
import MacScript.Prelude
import MacScript.Internal.Process (CarbonProcess(..), carbonProcess)
import MacScript.Internal.App (supportsAttributes, mkAppRetry)

import MacScript.Internal.App (App(..))

import Control.Monad.Loops (orM)

-- | Returns whether the two windows have the same identifier.
--
-- @
-- sameWID w1 w2 == (windowID w1 == windowID w2)
-- @
sameWID :: Window -> Window -> Bool
sameWID w1 w2 = _windowID w1 == _windowID w2

-- | Returns the window that is currently focused within an application.
--
-- Note that this may not be the window that has the current user input focus.
-- In particular, it is not if the specified application is not focused. Rather,
-- it is the window that would be focused if the specified parent application
-- had focus.
focusedWindowInApp
  :: (MonadIO m, MonadError e m, AsScriptError e) => App -> m (Maybe Window)
focusedWindowInApp app@(App {..}) = wrapAXErr . runMaybeT $ do
  el <- MaybeT $ maybeOnAXErrs [AXErrorNoValue]
          (attributeValue _appElement FocusedWindowAttribute)
  MaybeT (maybeOnInvalidOrTimeout (mkWindowRetry 1 app el))

-- | Returns the application that is currently focused.
focusedApp :: (MonadIO m, MonadError e m, AsScriptError e) => m App
focusedApp =
  liftIO (focusedProcess >>= carbonProcess) >>=
    fmap (fromMaybe err) . wrapAXErr . mkAppRetry 1
  where err = error "focusedApp: cannot create app"

--------------------------------------------------------------------------------
-- Windows

data Window = Window
  { _windowParent :: App
  , _windowElement :: UIElement
  , _windowID :: WindowID
  }

instance Show Window where
  show w = "Window (" ++ show (_windowParent w) ++ ") " ++ show (_windowID w)

mkWindowRetry
  :: MonadIO m => Int -> App -> UIElement -> ExceptT AXError m (Maybe Window)
mkWindowRetry n app ref =
  retryOnCannotComplete n $ do
    bAttrs <- supportsAttributes ref
      [ SomeAttribute RoleAttribute
      , SomeAttribute SubroleAttribute
      , SomeAttribute TitleAttribute
      , SomeAttribute FocusedAttribute
      , SomeAttribute PositionAttribute
      , SomeAttribute SizeAttribute
      , SomeAttribute MinimizedAttribute
      , SomeAttribute CloseButtonAttribute
      ]
    bRole <- (== WindowRole) <$> role ref
    if bAttrs && bRole
      then fmap Just (Window app ref <$> getWindowElementID ref)
      else pure Nothing

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

-- | Brings user focus to the specified application.
focusApp :: (MonadIO m, MonadError e m, AsScriptError e) => App -> m ()
focusApp app = do
  b <- liftIO . setFrontProcessFrontWindowOnly . _crbnPID . _appProcess $ app
  if b then pure () else throwing _ScriptError InvalidUIElementError

-- | Moves user input focus to the specified window.
focusWindow :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m ()
focusWindow w = do
  sp <- windowSpace w
  go
  orM (replicate 20 (go >> weGood sp)) >> pure ()
  where
    weGood sp = (&&) <$> onIt <*> onSameSpace sp
    currentlyFoc = focusedApp >>= focusedWindowInApp
    onIt = maybe False (sameWID w) <$> currentlyFoc
    onSameSpace s = (s ==) <$> activeSpace
    go = do
      focusApp (_windowParent w)
      wrapAXErr $ do
        setAttribute (_windowElement w) MainAttribute True
        setAttribute (_windowElement w) FocusedAttribute True

displayUUIDString :: DisplayID -> IO CFString
displayUUIDString = displayUUID >=> uuidString'

activeSpaceIDForDisplay :: DisplayID -> IO SpaceID
activeSpaceIDForDisplay = displayUUIDString >=> currentSpace'

activeSpaceID :: IO SpaceID
activeSpaceID = MacSdk.activeDisplay >>= activeSpaceIDForDisplay

-- | Return the space that is currently active.
activeSpace :: MonadIO m => m Space
activeSpace = liftIO (activeSpaceID >>= createSpace)

-- | Return the 'SpaceID' of the space that is currently active is the dispaly
-- with given 'DisplayID'.
activeSpaceForDisplay :: MonadIO m => DisplayID -> m Space
activeSpaceForDisplay = liftIO . (>>= createSpace) . activeSpaceIDForDisplay

-- | Returns the space to which the specified window belongs to.
windowSpace :: MonadIO m => Window -> m Space
windowSpace = liftIO . spaceForWindowID . _windowID
  where spaceForWindowID = spaceIDForWindowID >=> createSpace

-- | Returns the 'SpaceID' of the space to which the window with a given
-- 'WindowID' belongs to.
spaceIDForWindowID :: WindowID -> IO SpaceID
spaceIDForWindowID = displayForWindow' >=> currentSpace'

-- | Return all windows of an application that appear in the currently focused
-- Mission Control space.
windows :: (MonadIO m, MonadError e m, AsScriptError e) => App -> m [Window]
windows app@App {..} = do
  ws <- wrapAXErr . maybeOnAXErrs [ AXErrorNoValue ] $ do
    elems <- getWindowElements _appElement
    catMaybes <$> mapM (maybeOnInvalidOrTimeout . mkWindowRetry 1 app) elems
  maybe (pure []) pure ws
  where getWindowElements a =
          attributeValue a WindowsAttribute >>= liftIO . arrayValues

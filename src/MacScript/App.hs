-- | MacOS application management types and functions.

module MacScript.App
  ( PID
  , App
  -- * Query
  , appName
  , appPID
  , appPSN
  , interactiveApps
  , isAppHidden
  , findApp
  -- * Focus
  , focusApp
  , focusedApp
  -- * Events
  , windowCreatedEvent
  , focusedWindowChangedEvent
  , windowMovedEvent
  , windowResizedEvent
  , titleChangedEvent
  , windowMinimizedEvent
  , windowMaximizedEvent
  ) where

import MacSdk (PID, observerCreate, addNotification, observerStart',
               observerStop, observerRelease, UINotification(..))
import MacScript.Prelude
import MacScript.AppTypes
import MacScript.Process
import MacScript.Event
import MacScript.Error
import MacScript.Space

import MacScript.Internal.Process (CarbonProcess(..), carbonProcess)
import MacScript.Internal.App (App(..), mkAppRetry)
import MacScript.Internal.Window (Window(..), mkWindowRetry)

import Data.List (find)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Ptr
import Foreign.StablePtr
import Data.IORef

-- | Name of the application.
appName :: App -> String
appName = _appName

-- | PID of an application's associated process.
appPID :: App -> PID
appPID = _crbnPID . _appProcess

appPSN :: App -> PSN
appPSN = _crbnPSN . _appProcess

-- | Returns a list of all interactive applications in the system.
interactiveApps :: MonadIO m => m [App]
interactiveApps = do
  prcs <- interactiveProcs
  catMaybes <$> mapM (maybeOnInvalidOrTimeout' . mkAppRetry 1) prcs

-- | Finds the app with the specified name among the currently opened.
findApp :: MonadIO m => String -> m (Maybe App)
findApp name = fmap (find ((== name) . appName)) interactiveApps

-- | Returns whether the application is hidden or visible.
isAppHidden :: (MonadIO m, MonadError e m, AsScriptError e) => App -> m Bool
isAppHidden app =
  (liftIO . isProcessHidden . _appProcess) app >>=
    maybe (throwing _ScriptError InvalidUIElementError) pure

-- | Returns the application that is currently focused.
focusedApp :: (MonadIO m, MonadError e m, AsScriptError e) => m App
focusedApp =
  liftIO (focusedProcess >>= carbonProcess) >>=
    fmap (fromMaybe err) . wrapAXErr . mkAppRetry 1
  where err = error "focusedApp: cannot create app"

--------------------------------------------------------------------------------
-- App events

appE :: UINotification -> App -> Event Window
appE notif app = Event $ \h ->
  handleScriptErr (const (pure mempty)) . wrapAXErr $ do
    obs <- observerCreate (_crbnPID (_appProcess app)) $ \el notif' _ ->
      void . runMaybeT $ do
        guard (notif == notif')
        w <- MaybeT . maybeOnInvalidOrTimeout' $ mkWindowRetry 5 app el
        liftIO (h w)
    addNotification obs (_appElement app) notif (castPtrToStablePtr nullPtr)

    liftIO $ do
      observerStart' obs
      ref <- newIORef True
      let sub = Subscription $ atomicModifyIORef' ref (False,) >>= \b ->
            if b then observerStop obs else pure ()
      on_ (onceE $ filterE (== appPID app) appTerminatedEvent)
        (const (unsubscribe sub)) `onException` observerRelease obs
      pure sub

-- | Returns an event that fires when a new window is created within the
-- specified application.
windowCreatedEvent :: App -> Event Window
windowCreatedEvent = appE WindowCreatedNotification

-- | Returns an event that fires when the window with user input focus has
-- changed within the specified application.
--
-- As a consequence of how Mac OS event signalling works, this event fires
-- before the corresponding event from 'windowCreatedEvent' fires with a newly
-- created window.
focusedWindowChangedEvent :: App -> Event Window
focusedWindowChangedEvent = appE FocusedWindowChangedNotification

-- | Returns an event that fires when a window of the specified application has
-- changed its position.
windowMovedEvent :: App -> Event Window
windowMovedEvent = appE WindowMovedNotification

-- | Returns an event that fires when a window of the specified application has
-- changed its size.
windowResizedEvent :: App -> Event Window
windowResizedEvent = appE WindowResizedNotification

-- | Returns an event that fires when a window of the specified application has
-- changed its title.
titleChangedEvent :: App -> Event Window
titleChangedEvent = appE TitleChangedNotification

-- | Returns an event that fires when a window of the specified application has
-- been minimized.
windowMinimizedEvent :: App -> Event Window
windowMinimizedEvent = appE WindowMiniaturizedNotification

-- | Returns an event that fires when a window of the specified application was
-- minimized in the Dock and has been maximized.
--
-- When a maximized window pulls the focus to a different space from the
-- currently focused one, this event fires before the corresponding any event
-- signalling a space change.
windowMaximizedEvent :: App -> Event Window
windowMaximizedEvent = appE WindowDeminiaturizedNotification

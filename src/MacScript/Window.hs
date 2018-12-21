{-# LANGUAGE FlexibleContexts #-}

-- | MacOS window management types and functions.

module MacScript.Window
  ( WindowID
  , Window

  -- * Query
  , windowTitle
  , windowSpace
  , windowID
  , sameWID
  , windows
  , allWindowsInSpace
  , isValidWindow
  , validWindows
  , windowDisplay

  -- * Position/size attributes
  , setWindowRect
  , windowRect
  , windowMaximize
  , isWindowMovable
  , isWindowResizable
  , moveToSpace

  -- * Visibility attributes
  , isWindowMinimized
  , isWindowVisible
  , isWindowHidden

  -- * Other attributes
  , isWindowStandard
  , focusedWindow
  , windowParent
  , focusedWindowInApp
  , focusWindow
  , closeWindow

  -- * Events
  , windowDestroyedEvent
  ) where

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import MacSdk hiding (Event)
import MacScript.Prelude
import MacScript.AppTypes
import MacScript.App (focusedApp, isAppHidden, interactiveApps)
import MacScript.Event
import MacScript.Rectangle
import MacScript.Space
import MacScript.Display
import MacScript.Error
import MacScript.Internal.Process (CarbonProcess(..))
import MacScript.Internal.App (App(..))

import Foreign.Ptr
import Foreign.StablePtr
import Data.IORef

spaceMoveWindow :: MonadIO m => SpaceID -> WindowID -> m ()
spaceMoveWindow sid wid = liftIO $ do
  wid' <- numberCreate NumberIntType (fromIntegral wid)
  arr <- createArray' [wid']
  c <- defaultConnection
  moveWindowsToSpace c arr sid

-- | Move a window to a given space.
moveToSpace :: MonadIO m => Space -> Window -> m ()
moveToSpace sp w = -- do
  -- r <- windowRect w
  liftIO (spaceMoveWindow (spcID sp) (_windowID w))
  -- TODO: the following should be done, but only if the space we move the window to is in another display.
  -- whileM_ (fmap (== r) (windowRect w)) (liftIO (threadDelay 100))

-- | Retrieves the display containing the specified window.
--
-- Since a window could be placed in different displays at the same time, the
-- function compares the window's frame and that of all displays, and returns
-- the display with the biggest intersection.
windowDisplay :: (MonadError e m, AsScriptError e, MonadIO m) => Window -> m DisplayID
windowDisplay w = do
  fr <- windowRect w
  scs <- displays >>= \scrs -> zip scrs <$> mapM displayFullFrame scrs
  maybe (error "windowDisplay: no displays") (pure . fst) . headMay $
    sortBy (comparing (Down . area . intersection fr . snd)) scs

-- | Set the window frame to the maximum allowed by the display it belongs to.
windowMaximize :: (MonadError e m, AsScriptError e, MonadIO m) => Window -> m ()
windowMaximize w = windowDisplay w >>= (displayFrame >=> setWindowRect w)

-- | Return all windows that are placed in the currently focused Mission Control
-- space.
allWindowsInSpace :: MonadIO m => m [Window]
allWindowsInSpace =
  interactiveApps >>= fmap concat .
    mapM (handleScriptErr (const (pure [])) . windows)

setWindowPosition
  :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> Point -> m ()
setWindowPosition Window {..} pt =
  wrapAXErr (setAttribute _windowElement PositionAttribute pt)

setWindowSize
  :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> Size -> m ()
setWindowSize Window {..} sz =
  wrapAXErr (setAttribute _windowElement SizeAttribute sz)

-- | Returns the window position.
windowPosition
  :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m Point
windowPosition w =
  wrapAXErr (attributeValue (_windowElement w) PositionAttribute)

-- | Returns the window size.
windowSize :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m Size
windowSize w = wrapAXErr (attributeValue (_windowElement w) SizeAttribute)

-- | Returns whether the window is a valid window, where a /valid window/ is
-- defined to be one with non-zero ID.
--
-- @
-- isValidWindow w == windowID w /= 0
-- @
isValidWindow :: Window -> Bool
isValidWindow = (/= 0) . _windowID

-- | Returns all valid windows of an application that are "valid", according to
-- 'isValidWindow'.
validWindows :: (MonadIO m, MonadError e m, AsScriptError e) => App -> m [Window]
validWindows = fmap (filter isValidWindow) . windows

-- TODO: FIXME: alt+t 'ing and already tiled window tiles it again (leaving blank space)

-- | Event that fires when specified window is destroyed.
windowDestroyedEvent :: Window -> Event ()
windowDestroyedEvent (Window app el _) = onceE $ Event $ \h ->
  handleScriptErr (const (pure mempty)) . wrapAXErr $ do
    obs <- observerCreate (_crbnPID (_appProcess app)) $ \_ n _ -> case n of
      UIElementDestroyedNotification -> h ()
      _ -> pure ()
    addNotification obs el UIElementDestroyedNotification
      (castPtrToStablePtr nullPtr)

    liftIO $ do
      observerStart' obs
      -- wRef <- objRetain el
      ref <- newIORef True
      pure . Subscription $ atomicModifyIORef' ref (False,) >>= \b ->
        if b then observerRelease obs else pure ()
      --refRelease wRef `finally`

-- | ID of the window.
windowID :: Window -> WindowID
windowID = _windowID

-- | Returns the application to which the window belongs.
windowParent :: Window -> App
windowParent = _windowParent

-- | Closes a window. This effectively corresponds to pressing the red button on
-- the top left corner of the window.
closeWindow :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m ()
closeWindow w =
  wrapAXErr $ do
    b <- attributeValue (_windowElement w) CloseButtonAttribute
    actionPress b

-- | Returns whether a window can be moved.
isWindowMovable
  :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m Bool
isWindowMovable =
  wrapAXErr . flip isAttributeSettable PositionAttribute . _windowElement

-- | Returns whether a window can be resized.
isWindowResizable :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m Bool
isWindowResizable =
  wrapAXErr . flip isAttributeSettable SizeAttribute . _windowElement

-- | Returns whether a window is minimized.
isWindowMinimized :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m Bool
isWindowMinimized w =
  wrapAXErr $ attributeValue (_windowElement w) MinimizedAttribute

-- | Returns whether a window is hidden, i.e., if it is either minimized, or its
-- parent application is hidden. It returns 'Nothing' if the window element is
-- not valid.
isWindowHidden :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m Bool
isWindowHidden w = not <$> isWindowVisible w

-- | Returns whether a window is visible, i.e. if all of these conditions hold
--
-- * it is not minimized in the Dock
-- * its parent application is not hidden
-- * the window belongs to one of the currently active spaces
--   (one per visible display)
isWindowVisible :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m Bool
isWindowVisible w = fmap and . sequence $
  [ not <$> isWindowMinimized w
  , not <$> isAppHidden (windowParent w)
  , do spcs <- displays >>= mapM activeSpaceForDisplay
       or <$> mapM (`spaceHasWindow` w) spcs
  ]

-- | A window is standard whenever it corresponds to a full-fledged window with
-- a title bar. The same window may change the value of this attribute over time
-- (for example, a window becomes non-standard when minimized).
isWindowStandard :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m Bool
isWindowStandard w =
  wrapAXErr $ (== StandardWindowSubrole) <$> subrole (_windowElement w)

-- | Title of the window.
windowTitle
  :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m (Maybe String)
windowTitle w = wrapAXErr $
  attributeValue (_windowElement w) TitleAttribute >>= toString CFStringEncodingUTF8

-- | Frame of the window.
windowRect
  :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> m Rect
windowRect w = Rect <$> windowPosition w <*> windowSize w

-- | Sets the frame of the window.
setWindowRect
  :: (MonadIO m, MonadError e m, AsScriptError e) => Window -> Rect -> m ()
setWindowRect w Rect {..} =
  setWindowSize w size >> setWindowPosition w origin >> setWindowSize w size

-- | Returns the focused window within the currently focused application.
focusedWindow :: (MonadIO m, MonadError e m, AsScriptError e) => m (Maybe Window)
focusedWindow = focusedApp >>= focusedWindowInApp

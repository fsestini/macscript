module MacScript.WM.Event
  ( WMEvent(..)
  , events
  ) where

import MacScript
import MacScript.Prelude
import MacScript.WM.Core
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Reader

data WMEvent = NewWindow Window
             | WindowDestroyed Window
             | DisplayOrSpaceChanged
             | HotkeyDown Hotkey
             | VisibilityChanged

type EventM = ReaderT (TVar [Window]) Event

overWindows :: ([Window] -> [Window]) -> EventM ()
overWindows f = ask >>= liftIO . atomically . flip modifyTVar f

currentWindows :: EventM [Window]
currentWindows = ask >>= liftIO . atomically . readTVar

setupApp :: App -> EventM WMEvent
setupApp app = do
  putStr' "Setting up: " >> putStrLn' (appName app)
  wins <- handleUIErr (const []) $ windows app
  msum
    [ (msum . fmap setupNewWindow) wins
    , winCreat app >>= setupNewWindow
    , winMinMax app >> pure VisibilityChanged
    ]
  where
    winCreat a = lift (windowCreatedEvent a)
    winMinMax a = lift (windowMaximizedEvent a <|> windowMinimizedEvent a)

setupNewWindow :: Window -> EventM WMEvent
setupNewWindow w = do
  current <- currentWindows
  -- The check below is necessary because some apps (notably "Terminal")
  -- generate WindowCreatedEvent when a window is maximized.
  if windowID w `elem` fmap windowID current
    then lift neverE
    else overWindows (w :) >> (pure (NewWindow w) <|> winDestr)
  where
    winDestr = do
      lift (windowDestroyedEvent w)
      overWindows (filter ((/= windowID w) . windowID))
      pure (WindowDestroyed w)

refresh :: EventM WMEvent
refresh = do
  known <- currentWindows
  apps <- interactiveApps
  msum . flip fmap apps $ \app -> do
    current <- handleUIErr (const []) (windows app)
    let new = filter (\w -> windowID w `notElem` fmap windowID known) current
    msum (fmap setupNewWindow new)

events' :: [Hotkey] -> EventM WMEvent
events' hks = msum
  [ lift (displayChangedEvent <|> displayChangedEvent)
      >> (pure DisplayOrSpaceChanged <|> refresh)
  , lift appLaunchedEvent >>= setupApp
  , lift (fmap HotkeyDown (keyDownEvent hks))
  , lift appTerminatedEvent >>= \pid -> do
      toDelete <- filter ((== pid) . appPID . windowParent) <$> currentWindows
      msum (fmap (pure . WindowDestroyed) toDelete)
  , lift (appHiddenEvent <|> appVisibleEvent) >> pure VisibilityChanged
  ]

events :: [Hotkey] -> Event WMEvent
events kb = liftIO (newTVarIO []) >>= runReaderT (events' kb <|> initialSetup)
 where initialSetup = liftIO interactiveApps >>= msum . fmap setupApp

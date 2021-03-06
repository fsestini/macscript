module MacScript.WM where

import MacScript
import MacScript.WM.Event
import MacScript.WM.Core
import MacScript.WM.Operations
import MacScript.MenuBar

import Data.Maybe (fromMaybe)
import Control.Concurrent
import Control.Monad.State
import Data.List (nub)
import Control.Applicative ((<|>))

printEvent :: WMEvent -> WM ()
printEvent = \case
  NewWindow w -> do
    putStr' $ "new window: " ++ show (windowID w) ++ ":"
    t <- fromMaybe "<unknown>" <$> handleUIErr (const Nothing) (windowTitle w)
    putStrLn' t
  WindowDestroyed wid -> putStr' "window destroyed:" >> print' wid
  DisplayOrSpaceChanged -> putStrLn' "display/space changed"
  HotkeyDown _ -> putStrLn' "hotkey down"
  VisibilityChanged -> putStrLn' "visibility changed"

wmMain :: UserConfig -> IO ()
wmMain c = do
  item <- newStatusItem
  _ <- forkIO . runWM (WMConfig c item) $ do

    (SomeKeySet ks) <- _wmsKeySet <$> get
    setStatusItemTitle item (keySetStatusItem ks)

    runEventT $ onT_ (events keys) $ (flip (>>) . printEvent) <*> \case

      NewWindow w -> justLog "error processing new window: " $ do
        b <- lift $ (&&) <$> isWindowTileable w <*> configAllowsTile w
        if b then tile w else pure ()

      WindowDestroyed w ->
        justLog "error processing destroyed window: " (untile w)

      HotkeyDown hk -> do
        kbs <- _wmcKeybindings . _wmcUser <$> ask
        kss <- _wmcKeySets . _wmcUser <$> ask
        ks' <- _wmsKeySet <$> get
        fromMaybe (pure ()) (lookup hk kbs <|> (lookup ks' kss >>= lookup hk))
        renderTree

      VisibilityChanged -> renderTree
      DisplayOrSpaceChanged -> renderTree

  pure ()

  where
    keys = nub $ fmap fst (_wmcKeybindings c ++ concatMap snd (_wmcKeySets c))

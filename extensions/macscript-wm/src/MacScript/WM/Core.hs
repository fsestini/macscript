{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module MacScript.WM.Core
  ( WM
  , EWM
  , runWM
  , justLog
  , lift
  , liftIO
  , throwError
  , eToM
  , ask
  -- * Tiles
  , TiledWindow(..)
  , TileMode(..)
  , twWid
  , isWindowTileable
  -- , overWorkspace
  , overTiles
  , overLayout
  , tilesInSpace
  , layout
  , workspace
  , checkS
  -- * Messages
  , castMsg
  , Message
  , SomeMessage(..)
  , Layout(..)
  , SomeLayout(..)
  , Direction(..)
  , StdMessage(..)
  -- * Key sets
  , KeySet(..)
  , SomeKeySet(..)
  , StandardKeySet(..)
  , wmsKeySet
  -- * Configuration
  , UserConfig(..)
  , WMConfig(..)
  -- * State
  , WMState(..)
  , wmsTiled
  , wmsSpaceLayouts
  -- * Error
  , WMError(..)
  , handleUIErr
  , handleUIErrM
  ) where

import MacScript
import MacScript.Error
import MacScript.MenuBar

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Typeable (Typeable, cast)
import Utils
import Data.List.NonEmptyZipper
import Data.List.NonEmpty (NonEmpty(..))
import Lens.Micro (Lens', _Just, lens, at)
import Lens.Micro.Mtl ((%=), preuse, use, (.=))
import Lens.Micro.GHC ()

handleUIErr :: Functor m => (ScriptError -> a) -> ExceptT ScriptError m a -> m a
handleUIErr f = fmap (either f id) . runExceptT

handleUIErrM :: Monad m => (ScriptError -> m a) -> ExceptT ScriptError m a -> m a
handleUIErrM f = join . fmap (either f pure) . runExceptT

--------------------------------------------------------------------------------
-- Tiles

data TiledWindow = TW
  { _twWindow :: Window
  , _twFloatRect :: Maybe Rect
  }

twWid :: TiledWindow -> WindowID
twWid = windowID . _twWindow

data TileMode = OptIn [String] | OptOut [String]

--------------------------------------------------------------------------------
-- WM messages and layouts

class Typeable m => Message (m :: * -> *)

data SomeMessage a = forall m . Message m => SomeMessage (m a)

castMsg :: (Typeable a, Message m)
        => SomeMessage a -> Maybe (m a)
castMsg (SomeMessage m) = cast m

data Direction = DirLeft | DirRight | DirUp | DirDown | DirNext | DirPrev

data StdMessage :: * -> * where
  FocusTowards :: Int -> Int -> Direction -> StdMessage Int
  InsertNew :: StdMessage Int
  SwapTowards :: NonEmptyZipper WindowID -> Direction -> StdMessage (Int, Int)
  deriving Typeable

instance Message StdMessage

class Layout l where
  render :: Rect -> l -> [TiledWindow] -> [(Window, Rect)]
  handleMsg :: SomeMessage a -> l -> WM (Maybe (l, a))
  -- handleMsg :: SomeMessage -> l -> [TiledWindow] -> WM (Maybe (l, [TiledWindow]))

data SomeLayout = forall l . Layout l => SomeLayout l

--------------------------------------------------------------------------------

class (Typeable a, Eq a) => KeySet a where
  keySetStatusItem :: a -> String

data SomeKeySet = forall a . KeySet a => SomeKeySet a

instance Eq SomeKeySet where
  SomeKeySet ks == SomeKeySet ks' = cast ks == Just ks'

data StandardKeySet
  = NormalKeySet
  | WindowKeySet
  | MouseKeySet
  deriving (Typeable, Eq, Show)

instance KeySet StandardKeySet where
  keySetStatusItem NormalKeySet = "N"
  keySetStatusItem WindowKeySet = "W"
  keySetStatusItem MouseKeySet = "M"

--------------------------------------------------------------------------------

data UserConfig = UserConfig
  { _wmcTileMode    :: TileMode
  , _wmcPadding     :: Double
  , _wmcLayouts     :: NonEmpty (String, SomeLayout)
  , _wmcKeybindings :: [(Hotkey, WM ())]
  , _wmcKeySets     :: [(SomeKeySet, [(Hotkey, WM ())])]
  }

data WMConfig = WMConfig
  { _wmcUser :: UserConfig
  , _wmcStatusItem :: StatusItem
  }

data WMState = WMState
  { _wmsSpaceLayouts :: Map SpaceID (NonEmptyZipper SomeLayout)
  , _wmsTiled :: [TiledWindow]
  , _wmsKeySet :: SomeKeySet
  }

wmsTiled :: Lens' WMState [TiledWindow]
wmsTiled = lens _wmsTiled (\s tws -> s { _wmsTiled = tws })

wmsSpaceLayouts :: Lens' WMState (Map SpaceID (NonEmptyZipper SomeLayout))
wmsSpaceLayouts = lens _wmsSpaceLayouts (\s x -> s { _wmsSpaceLayouts = x })

wmsKeySet :: Lens' WMState SomeKeySet
wmsKeySet = lens _wmsKeySet (\s ks -> s { _wmsKeySet = ks })

data WMError
--  = WinNotManaged WindowID
  = NoDisplayIdx Int
  | UnknownSpace SpaceID
  | NoWindowsInSpace
  | UIError ScriptError
  deriving (Show)

instance AsScriptError WMError where
  _ScriptError =
    prism (\e -> case e of { UIError e' -> Right e' ; _ -> Left e }) UIError

newtype WM a = WM { unWM :: ReaderT WMConfig (StateT WMState IO) a }
  deriving ( Functor , Applicative , Monad
           , MonadReader WMConfig , MonadState WMState, MonadIO
           )

type EWM = ExceptT WMError WM

runWM :: WMConfig -> WM a -> IO a
runWM c =
  fmap fst . flip runStateT (WMState mempty [] (SomeKeySet NormalKeySet)) .
    flip runReaderT c . unWM

eToM :: EWM a -> WM (Maybe a)
eToM = fmap (either (const Nothing) Just) . runExceptT

justLog :: String -> EWM () -> WM ()
justLog s =
  join . fmap (either (\e -> liftIO (putStr s >> print e)) pure) . runExceptT

--------------------------------------------------------------------------------

isWindowTileable :: Window -> WM Bool
isWindowTileable w = handleUIErr (const False) $
  and <$> sequence [isWindowStandard w, isWindowMovable w, isWindowResizable w]

layout :: Space -> EWM SomeLayout
layout sp = do
  lift (checkS (spcID sp))
  mly <- preuse (wmsSpaceLayouts . at (spcID sp) . _Just . current)
  maybe (throwError (UnknownSpace (spcID sp))) pure mly

overLayout :: Space -> (forall l. Layout l => l -> EWM (Maybe (l, a)))
           -> EWM (Maybe a)
overLayout sp f = do
  (SomeLayout l) <- layout sp
  m <- f l
  flip (maybe (pure Nothing)) m $ \(newL, x) -> do
    wmsSpaceLayouts . at (spcID sp) . _Just . current .= SomeLayout newL
    pure (Just x)

tilesInSpace :: Space -> WM [TiledWindow]
tilesInSpace sp = do
  allTiled <- use wmsTiled
  tiledInSpace <- liftIO (filterM (spaceHasWindow sp . _twWindow) allTiled)
  filterM (handleUIErr (const False) . isWindowVisible . _twWindow) tiledInSpace

overTiles :: Space -> ([TiledWindow] -> EWM (Maybe [TiledWindow]))
          -> EWM ()
overTiles sp f = do
  tws <- lift (tilesInSpace sp)
  m <- f tws
  flip (maybe (pure ())) m $ \newTws ->
    wmsTiled %= overSubstring ((`elem` fmap twWid tws) . twWid) (const newTws)

-- overWorkspace
--   :: Space
--   -> (forall l. Layout l => l -> [TiledWindow] -> EWM (Maybe (l, [TiledWindow])))
--   -> EWM ()
-- overWorkspace sp f = do
--   (SomeLayout l, tws) <- workspace sp
--   m <- f l tws
--   flip maybeM m $ \(newL, newTws) -> do
--     wmsSpaceLayouts . at (spcID sp) . _Just . current .= SomeLayout newL
--     wmsTiled %= overSubstring ((`elem` fmap twWid tws) . twWid) (const newTws)

workspace :: Space -> EWM (SomeLayout, [TiledWindow])
workspace sp = (,) <$> layout sp <*> lift (tilesInSpace sp)

-- workspace :: Space -> EWM (SomeLayout, [TiledWindow])
-- workspace sp = do
--   lift (checkS (spcID sp))
--   allTiled <- use wmsTiled
--   tiledInSpace <- liftIO (filterM (spaceHasWindow sp . _twWindow) allTiled)
--   visibleTiles <-
--     filterM (handleUIErr (const False) . isWindowVisible . _twWindow) tiledInSpace
--   mly <- preuse (wmsSpaceLayouts . at (spcID sp) . _Just . current)
--   maybe (throwError (UnknownSpace (spcID sp))) (pure . (,visibleTiles)) mly

checkS :: SpaceID -> WM ()
checkS sid = fmap (fmap snd . _wmcLayouts . _wmcUser) ask >>= checkS' sid
  where
    checkS' sid' l = wmsSpaceLayouts . at sid' %= Just . fromMaybe (fromNonEmpty l)

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleInstances #-}

{-|

Compositional, monadic event sources, offering a flexible interface for easy
composition and manipulation. The instances of @Functor@, @Applicative@, and
@Alternative@ for @Event@ allow easy transformation of the event sources. The
instance of @Monad@ allows to compose and chain several event sources into one.

-}

module MacScript.Event
  ( Event(..)
  , unsubscribe

  -- * Subscribing and unsubscribing
  , Subscription(..)
  , on
  , on_
  , MonadUnliftIO(..)
  , askRunInIO

  -- * Event combinators
  , ioE
  , neverE
  , onceE
  , dropE
  , takeE
  , filterE
  ) where

import MacScript.Internal.Prelude
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, modifyTVar)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift
import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (ap, MonadPlus(..))

newtype Subscription = Subscription { _subDispose :: IO () }
  deriving (Semigroup, Monoid)

-- | Type of events that produce a stream of elements of type @a@. Attempts to
-- connected an handler may fail with error type @e@.
newtype Event a = Event { runEvent :: (a -> IO ()) -> IO Subscription }

-- | Creates an event source from an IO action. When subscribed, this event
-- source executes the IO action once and calls the event handler with its
-- result.
ioE :: IO a -> Event a
ioE m = Event $ \k -> m >>= \x -> k x >> pure mempty

instance Functor Event where
  fmap f (Event e) = Event $ \k -> e (k . f)
instance Applicative Event where
  pure = return
  (<*>) = ap
instance Alternative Event where
  empty = Event $ \_ -> return mempty
  e1 <|> e2 = Event $ \k ->
    liftA2 (<>) (on e1 (liftIO . k)) (on e2 (liftIO . k))
instance MonadPlus Event where
  mzero = empty
  mplus = (<|>)
instance Monad Event where
  return x = Event $ \k -> liftIO (k x >> pure mempty)
  e >>= f = Event $ \k -> do
    dref <- liftIO (newTVarIO mempty)
    morph <- askRunInIO
    liftIO $ addD morph dref e $ \x -> addD morph dref (f x) k
    liftIO (readTVarIO dref)
    where
      addD f' v e' h = do
        d' <- f' (runEvent e' h)
        atomically (modifyTVar v (<> d'))

instance MonadIO Event where
  liftIO = ioE . liftIO

-- | An event that never fires
neverE :: Event a
neverE = Event $ const (pure mempty)

-- | Unsubscribes from an event subscription, disconnecting the handler and
-- freeing resources.
unsubscribe :: MonadIO m => Subscription -> m ()
unsubscribe = liftIO . _subDispose

on :: MonadUnliftIO m => Event a -> (a -> m ()) -> m Subscription
on (Event e) f = askRunInIO >>= \morph -> (liftIO . e) (morph . f)

on_ :: MonadUnliftIO m => Event a -> (a -> m ()) -> m ()
on_ e f = on e f >> pure ()

-- | Turns an event source into one that only fires once, with the first element
-- that is produced by the original source.
onceE :: Event a -> Event a
onceE e = Event $ \k -> do
  rec d <- on e $ \x -> do
        unsubscribe d
        liftIO (k x)
  pure d

-- | Filter an event source according to a specified predicate.
filterE :: (a -> Bool) -> Event a -> Event a
filterE p (Event e) = Event $ \k -> e (\x -> if p x then k x else pure ())

-- | @takeE n e@ turns @e@ into an event source that only produces the first @n@
-- elements of @e@.
takeE :: Int -> Event a -> Event a
takeE 0 _ = empty
takeE 1 e = onceE e
takeE n e | n > 1 = onceE e <|> takeE (n - 1) e
          | otherwise = error "takeE: n must be non-negative"

-- | @dropE n e@ drops the first @n@ elements from @e@, and fires the ones that
-- follow.
dropE :: Int -> Event a -> Event a
dropE n e = replicateM_ n (onceE e) >> e

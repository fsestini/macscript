{-# LANGUAGE RankNTypes #-}

module MacScript.Error where

import MacSdk (AXError(..), CGError(..))
import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (runExceptT, ExceptT(..), lift)
import qualified Control.Monad.Except as E (throwError)

import Data.Functor.Identity (Identity(..))
import Data.Profunctor.Choice (Choice(..))
import Data.Profunctor (Profunctor(..))
import Control.Monad.Error.Class (MonadError(..))
import Data.Tagged (Tagged(..))

type Prism' s a =
  forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)

type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

review :: AReview t b -> b -> t
review r = runIdentity . unTagged . r . Tagged . Identity

throwing :: MonadError t m => Prism' t e -> e -> m a
throwing p e = throwError (review p e)

prism :: (s -> Either s a) -> (a -> s) -> Prism' s a
prism prev rev p = dimap prev (either pure (fmap rev)) (right' p)

-- | Datatype of errors that are thrown when performing operations on UI
-- elements. These kinds of errors are actually not rare when managing UI
-- elements, so they must be handled explicitly.
data ScriptError
  = InvalidUIElementError
  -- ^ The UI element is invalid. This error usually originates from requests
  -- that are performed on UI elements that have been destroyed (like a closed
  -- window or a terminated app) and are not available anymore.
  | RequestTimeoutError
  -- ^ The requested action could not be completed. This could be due to an
  -- unresponsive UI element.
  deriving Show

-- | Typeclass of error types that can represent 'ScriptError' errors.
class AsScriptError t where
  _ScriptError :: Prism' t ScriptError

instance AsScriptError ScriptError where
  _ScriptError = id

handleScriptErr :: Monad m => (ScriptError -> m a) -> ExceptT ScriptError m a -> m a
handleScriptErr f = join . fmap (either f pure) . runExceptT

wrapAXErr :: (MonadError e m, AsScriptError e) => ExceptT AXError m a -> m a
wrapAXErr = join . fmap (either thro pure) . runExceptT
  where
    thro AXErrorCannotComplete = throwing _ScriptError RequestTimeoutError
    thro AXErrorInvalidUIElement = throwing _ScriptError InvalidUIElementError
    thro e = error ("AXError: " ++ show e)

wrapCGErr :: Monad m => ExceptT CGError m a -> m a
wrapCGErr =
  join . fmap (either (error . ("CGError: " ++) . show) pure) . runExceptT

retryOnCannotComplete :: MonadIO m => Int -> ExceptT AXError m a -> ExceptT AXError m a
retryOnCannotComplete 0 _ = E.throwError AXErrorCannotComplete
retryOnCannotComplete n m = do
  res <- lift (runExceptT m)
  either
    (\e ->
       if e == AXErrorCannotComplete
         then liftIO (threadDelay 1000000) >> retryOnCannotComplete (n - 1) m
         else E.throwError e) pure res

maybeOnAXErrs :: Functor m => [AXError] -> ExceptT AXError m a -> ExceptT AXError m (Maybe a)
maybeOnAXErrs errs =
  ExceptT . fmap (either
    (\e ->
      if e `elem` errs
        then pure Nothing
        else E.throwError e) (pure . Just)) . runExceptT

maybeOnInvalidOrTimeout
  :: Functor m => ExceptT AXError m (Maybe a) -> ExceptT AXError m (Maybe a)
maybeOnInvalidOrTimeout =
  fmap join . maybeOnAXErrs [ AXErrorCannotComplete, AXErrorInvalidUIElement ]

maybeOnInvalidOrTimeout'
  :: Functor m => ExceptT AXError m (Maybe a) -> m (Maybe a)
maybeOnInvalidOrTimeout' =
  fmap (either (error . ("AXError: " ++) . show) join) .
    runExceptT . maybeOnAXErrs [ AXErrorCannotComplete, AXErrorInvalidUIElement ]

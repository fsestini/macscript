{-# LANGUAGE FlexibleContexts #-}

module MacScript.AppTypes where

-- import MacSdk

-- import MacScript.Prelude
import MacScript.Internal.Window (Window(..))

-- | Returns whether the two windows have the same identifier.
--
-- @
-- sameWID w1 w2 == (windowID w1 == windowID w2)
-- @
sameWID :: Window -> Window -> Bool
sameWID w1 w2 = _windowID w1 == _windowID w2

-- startObserver :: Observer -> IO ()
-- startObserver obs = do
--   mrl <- mainRunLoop
--   rls <- observerGetRunLoopSource obs
--   b <- runLoopContainsSource mrl rls DefaultMode
--   if b then pure () else runLoopAddSource mrl rls DefaultMode

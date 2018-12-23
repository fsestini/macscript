{-# OPTIONS_GHC -Wno-orphans #-}

-- | Convenience module defining an orphan instance of 'AsScriptError' for
-- 'IOException'. It is useful when using MacScript from ghci, as it allows to
-- call any 'ScriptError'-throwing function from the IO monad. Regular code
-- should definitely not use this, and instead handle 'ScriptError' in the
-- intended way.
--
-- Example session:
--
-- >>> import MacScript.Interactive
-- >>> Just app <- findApp "Emacs"
-- >>> [w] <- windows app
-- >>> windowTitle w
-- Just "Interactive.hs - Emacs"
-- >>> windowSpace w
-- Space {spcID = 6, spcType = UserSpace}

module MacScript.Interactive (module MacScript) where

import MacScript

import MacScript.Internal.Error (prism)
import Control.Exception

instance AsScriptError IOException where
  _ScriptError = prism Left (\e -> userError ("MacScript error: " ++ show e))

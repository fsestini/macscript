{- |

== Basic queries

The following prints the name of the applications that are currently opened.

>>> import MacScript.Interactive
>>> fmap appName <$> interactiveApps >>= mapM_ putStrLn
Finder
Safari
Emacs
...

This instead prints the title (if they have one) of all windows belonging to the
application named Emacs:

>>> Just emacs <- findApp "Emacs"
>>> windows emacs >>= mapM windowTitle >>= mapM_ print
Just "macscript - Emacs"
Just "README.md - Emacs"

== Events

The following snippet registers for keyboard events, and prints the title of the
currently focused window every time the user presses left alt + left shift + the
B key.

> main = runScript $
>   on (keyDownEvent [lalt <+> key 'b']) $ \_ ->
>     focusedWindow >>= mapM_ (windowTitle >=> print)

== Window movement

The following short script shows some basic window manipulation capabilities of
MacScript. When run, it moves the currently focused window to the left half
portion of the screen upon pressing alt + H, and to the right half upon pressing
alt + L.

> main = runScript $
>
>   on_ (keyDownEvent [lalt <+> key 'h']) $ \_ ->
>     mapM_ focusedWindow $ \w -> do
>       r <- windowDisplay w >>= displayFrame
>       setWindowRect w (fst (splitVertically 0.5 r))
>
>   on_ (keyDownEvent [lalt <+> key 'l']) $ \_ ->
>     mapM_ focusedWindow $ \w -> do
>       r <- windowDisplay w >>= displayFrame
>       setWindowRect w (snd (splitVertically 0.5 r))

-}

module MacScript
  ( module MacScript.App
  , module MacScript.Display
  , module MacScript.Process
  , module MacScript.Space
  , module MacScript.Mouse
  , module MacScript.Event
  , module MacScript.Window
  , module MacScript.Keyboard
  , module MacScript.Rectangle
  , module MacScript.Internal.Error
  , module MacScript.Load
  , module Control.Monad.IO.Lifted
  ) where

import MacScript.App
import MacScript.Display
import MacScript.Process
import MacScript.Space
import MacScript.Window
import MacScript.Keyboard
import MacScript.Rectangle
import MacScript.Mouse
import MacScript.Event
import MacScript.Load
import MacScript.Internal.Error (ScriptError(..), AsScriptError(..))
import Control.Monad.IO.Lifted

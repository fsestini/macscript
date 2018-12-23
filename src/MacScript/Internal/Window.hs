module MacScript.Internal.Window where

import MacSdk (UIElement, WindowID, AXError, Role(..), role, getWindowElementID)
import MacSdk.Framework.Accessibility.Attribute.Types
  (SomeAttribute(..), Attribute(..))

import MacScript.Internal.Prelude
import MacScript.Internal.Error (retryOnCannotComplete)
import MacScript.Internal.App (App, supportsAttributes)

data Window = Window
  { _windowParent :: App
  , _windowElement :: UIElement
  , _windowID :: WindowID
  }

instance Show Window where
  show w = "Window (" ++ show (_windowParent w) ++ ") " ++ show (_windowID w)

mkWindowRetry
  :: MonadIO m => Int -> App -> UIElement -> ExceptT AXError m (Maybe Window)
mkWindowRetry n app ref =
  retryOnCannotComplete n $ do
    bAttrs <- supportsAttributes ref
      [ SomeAttribute RoleAttribute
      , SomeAttribute SubroleAttribute
      , SomeAttribute TitleAttribute
      , SomeAttribute FocusedAttribute
      , SomeAttribute PositionAttribute
      , SomeAttribute SizeAttribute
      , SomeAttribute MinimizedAttribute
      , SomeAttribute CloseButtonAttribute
      ]
    bRole <- (== WindowRole) <$> role ref
    if bAttrs && bRole
      then fmap Just (Window app ref <$> getWindowElementID ref)
      else pure Nothing

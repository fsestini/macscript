module MacSdk.Framework.AppKit.CoreApp.Application where

import Foreign
import Foreign.C.Types (CBool(..), CInt(..))
import Control.Monad.IO.Class (MonadIO(..))
import MacSdk.Framework.NSObject
import MacSdk.Framework.Types (PID)

data NSApplication
instance NSClass NSApplication
newtype Application = Application { getApplication :: ForeignPtr NSApplication }
  deriving NSObject

foreign import ccall nsApplication_sharedApplication :: IO (Ptr NSApplication)
foreign import ccall "nsApp" nsApp_ :: IO (Ptr NSApplication)
foreign import ccall app_run :: Ptr NSApplication -> IO ()
foreign import ccall app_stop :: Ptr NSApplication -> Ptr a -> IO ()
foreign import ccall app_terminate :: Ptr NSApplication -> Ptr a -> IO ()
foreign import ccall "NSApplicationLoad" ns_application_load :: IO CBool
foreign import ccall newAutoreleasePool_ :: IO (Ptr NSAutoreleasePool)

-- | Startup function to call when running Cocoa code from a Carbon application.
applicationLoad :: MonadIO m => m Bool
applicationLoad = liftIO (toBool <$> ns_application_load)

data NSAutoreleasePool
instance NSClass NSAutoreleasePool
newtype AutoreleasePool = AutoreleasePool
  { getPool :: ForeignPtr NSAutoreleasePool
  } deriving NSObject

newAutoreleasePool :: MonadIO m => m AutoreleasePool
newAutoreleasePool = liftIO (newAutoreleasePool_ >>= manageNSPtr)

foreign import ccall unsafe "NSApplicationActivationPolicyRegular_"
  nsApplicationActivationPolicyRegular_ :: NSApplicationActivationPolicy
foreign import ccall unsafe "NSApplicationActivationPolicyAccessory_"
  nsApplicationActivationPolicyAccessory_ :: NSApplicationActivationPolicy
foreign import ccall unsafe "NSApplicationActivationPolicyProhibited_"
  nsApplicationActivationPolicyProhibited_ :: NSApplicationActivationPolicy

data ActivationPolicy
  = ActivationPolicyRegular
  | ActivationPolicyAccessory
  | ActivationPolicyProhibited

toNSPolicy :: ActivationPolicy -> NSApplicationActivationPolicy
toNSPolicy = \case
  ActivationPolicyRegular -> nsApplicationActivationPolicyRegular_
  ActivationPolicyAccessory -> nsApplicationActivationPolicyAccessory_
  ActivationPolicyProhibited -> nsApplicationActivationPolicyProhibited_

type NSApplicationActivationPolicy = Word32
foreign import ccall unsafe set_activation_policy
  :: Ptr NSApplication -> NSApplicationActivationPolicy -> IO ()
foreign import ccall unsafe activate_ignoring_other_apps
  :: Ptr NSApplication -> CBool -> IO ()

appSetActivationPolicy :: MonadIO m => Application -> ActivationPolicy -> m ()
appSetActivationPolicy app p =
  liftIO (withNSPtr app (flip set_activation_policy (toNSPolicy p)))

appActivateIgnoringOtherApps :: MonadIO m => Application -> Bool -> m ()
appActivateIgnoringOtherApps app b =
  liftIO (withNSPtr app (flip activate_ignoring_other_apps (fromBool b)))

-- | Returns the application instance, creating it if it doesn’t exist yet.
sharedApplication :: MonadIO m => m (Ptr NSApplication)
sharedApplication = liftIO nsApplication_sharedApplication

-- | The shared app instance.
nsApp :: MonadIO m => m Application
nsApp = liftIO (nsApp_ >>= manageNSPtr)

-- | Starts the main event loop.
appRun :: MonadIO m => Application -> m ()
appRun = liftIO . flip withNSPtr app_run

appStop :: (MonadIO m, NSObject obj) => Application -> Maybe obj -> m ()
appStop app mobj =
  liftIO $ withNSPtr app $ \p ->
    maybe (app_stop p nullPtr) (flip withNSPtr (app_stop p)) mobj

appTerminate :: (MonadIO m, NSObject obj) => Application -> Maybe obj -> m ()
appTerminate app mobj =
  liftIO $ withNSPtr app $ \p ->
    maybe (app_terminate p nullPtr) (flip withNSPtr (app_terminate p)) mobj

foreign import ccall unsafe processIdentifier_ :: IO CInt

processIdentifier :: MonadIO m => m PID
processIdentifier = liftIO (fmap fromIntegral processIdentifier_)

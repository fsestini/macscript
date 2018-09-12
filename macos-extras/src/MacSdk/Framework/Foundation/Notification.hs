-- |

module MacSdk.Framework.Foundation.Notification where

import MacSdk.Framework.Foundation.Object
import MacSdk.Framework.Foundation.String
import MacSdk.Framework.Foundation.Workspace
import MacSdk.Framework.Foundation.OperationQueue
import MacSdk.Framework.Types
import Foreign
import Control.Monad.IO.Class (MonadIO(..))
import System.IO.Unsafe

data NSNotification
type NSNotificationName = Ptr NSString
newtype Notification = Notification { getNotification :: Ptr NSNotification }

data NSNotificationCenter
newtype NotificationCenter = NotificationCenter
  { getNotificationCenter :: Ptr NSNotificationCenter
  }

foreign import ccall unsafe "NSWorkspaceActiveDisplayDidChangeNotification_"
  nsWorkspaceActiveDisplayDidChangeNotification :: IO NSNotificationName
foreign import ccall unsafe "NSWorkspaceActiveSpaceDidChangeNotification_"
  nsWorkspaceActiveSpaceDidChangeNotification :: IO NSNotificationName
foreign import ccall unsafe "NSWorkspaceDidActivateApplicationNotification_"
  nsWorkspaceDidActivateApplicationNotification :: IO NSNotificationName
foreign import ccall unsafe "NSWorkspaceDidDeactivateApplicationNotification_"
  nsWorkspaceDidDeactivateApplicationNotification :: IO NSNotificationName
foreign import ccall unsafe "NSWorkspaceDidHideApplicationNotification_"
  nsWorkspaceDidHideApplicationNotification :: IO NSNotificationName
foreign import ccall unsafe "NSWorkspaceDidUnhideApplicationNotification_"
  nsWorkspaceDidUnhideApplicationNotification :: IO NSNotificationName

foreign import ccall unsafe "NSWorkspaceDidLaunchApplicationNotification_"
  nsWorkspaceDidLaunchApplicationNotification :: IO NSNotificationName
foreign import ccall unsafe "NSWorkspaceDidTerminateApplicationNotification_"
  nsWorkspaceDidTerminateApplicationNotification :: IO NSNotificationName

data NotificationName
  = WorkspaceActiveDisplayDidChangeNotification
  | WorkspaceActiveSpaceDidChangeNotification
  | WorkspaceDidActivateApplicationNotification
  | WorkspaceDidDeactivateApplicationNotification
  | WorkspaceDidHideApplicationNotification
  | WorkspaceDidUnhideApplicationNotification
  | WorkspaceDidLaunchApplicationNotification
  | WorkspaceDidTerminateApplicationNotification

toNotificationString :: NotificationName -> NSNotificationName
toNotificationString = \case
  WorkspaceActiveDisplayDidChangeNotification ->
    unsafePerformIO nsWorkspaceActiveDisplayDidChangeNotification
  WorkspaceActiveSpaceDidChangeNotification ->
    unsafePerformIO nsWorkspaceActiveSpaceDidChangeNotification
  WorkspaceDidActivateApplicationNotification ->
    unsafePerformIO nsWorkspaceDidActivateApplicationNotification
  WorkspaceDidDeactivateApplicationNotification ->
    unsafePerformIO nsWorkspaceDidDeactivateApplicationNotification
  WorkspaceDidHideApplicationNotification ->
    unsafePerformIO nsWorkspaceDidHideApplicationNotification
  WorkspaceDidUnhideApplicationNotification ->
    unsafePerformIO nsWorkspaceDidUnhideApplicationNotification
  WorkspaceDidLaunchApplicationNotification ->
    unsafePerformIO nsWorkspaceDidLaunchApplicationNotification
  WorkspaceDidTerminateApplicationNotification ->
    unsafePerformIO nsWorkspaceDidTerminateApplicationNotification

foreign import ccall unsafe notif_to_pid :: Ptr NSNotification -> IO PID

notificationPID :: MonadIO m => Notification -> m PID
notificationPID = liftIO . notif_to_pid . getNotification

foreign import ccall unsafe
  notification_center :: Ptr NSWorkspace -> IO (Ptr NSNotificationCenter)

notificationCenter :: MonadIO m => Workspace -> m NotificationCenter
notificationCenter =
  liftIO . fmap NotificationCenter . notification_center . getWorkspace

type NSNotificationCallback = Ptr NSNotification -> IO ()
type NotificationCallback = Notification -> IO ()

foreign import ccall "wrapper"
  wrap_notif_callb :: NSNotificationCallback -> IO (FunPtr NSNotificationCallback)

foreign import ccall unsafe
  add_observer_for_name :: Ptr NSNotificationCenter
                        -> NSNotificationName
                        -> Ptr NSObject
                        -> Ptr NSOperationQueue
                        -> FunPtr NSNotificationCallback
                        -> IO (Ptr NSObject)

addObserverForName
  :: MonadIO m
  => NotificationCenter
  -> NotificationName
  -> Object
  -> OperationQueue
  -> NotificationCallback
  -> m Object
addObserverForName center name obj queue callb = do
  callb' <- liftIO $ wrap_notif_callb (\p -> callb (Notification p))
  liftIO . fmap Object $
    add_observer_for_name
      (getNotificationCenter center)
      (toNotificationString name)
      (getObject obj)
      (getOperationQueue queue)
      callb'

foreign import ccall unsafe
  remove_observer :: Ptr NSNotificationCenter -> Ptr NSObject -> IO ()

removeObserver :: MonadIO m => NotificationCenter -> Object -> m ()
removeObserver center =
  liftIO . remove_observer (getNotificationCenter center) . getObject

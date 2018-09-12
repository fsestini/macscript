#include <Cocoa/Cocoa.h>

bool is_proc_hidden(pid_t PID, bool *result) {
  NSRunningApplication *app =
    [NSRunningApplication runningApplicationWithProcessIdentifier:PID];
  if (app) {
    *result = [app isHidden];
    return true;
  } else {
    return false;
  }
}

// Adapted from https://github.com/koekeishiya/chunkwm

char *proc_name_and_policy(pid_t PID, uint32_t *policy) {
  char *name = NULL;
  NSRunningApplication *app =
    [NSRunningApplication runningApplicationWithProcessIdentifier:PID];
  if (app) {
    *policy = [app activationPolicy];

    const char *appName = [[app localizedName] UTF8String];
    if (appName) { name = strdup(appName); }
  }

  return name;
}

typedef void (workspace_callback_t)(uint32_t, pid_t);

typedef void (nsnotification_callback_t)(NSNotification *);

@interface WorkspaceWatcher : NSObject { }
- (id)initWithCallback:(workspace_callback_t *)callback;
@end

WorkspaceWatcher *new_workspace_watcher(workspace_callback_t *callback) {
  return [[WorkspaceWatcher alloc] initWithCallback: callback];
}

void stop_workspace_watcher(WorkspaceWatcher *watcher) {
  [watcher dealloc];
}

pid_t notif_to_pid(NSNotification *notif) {
  return [[[notif userInfo] valueForKey:NSWorkspaceApplicationKey]
	   processIdentifier];
}

NSWorkspace *shared_workspace() {
  return [NSWorkspace sharedWorkspace];
}

NSNotificationCenter *notification_center(NSWorkspace *workspace) {
  return [workspace notificationCenter];
}

NSOperationQueue *main_queue() {
  return [NSOperationQueue mainQueue];
}

NSObject *add_observer_for_name(
  NSNotificationCenter *center,
  NSString *name,
  NSObject *obj,
  NSOperationQueue *q,
  nsnotification_callback_t *callb) {
  return [center addObserverForName: name
                             object: obj
                              queue: q
                         usingBlock: ^(NSNotification *n) {
      // pid_t pid = [[[n userInfo] valueForKey:NSWorkspaceApplicationKey] processIdentifier];
      // NSLog(@"PID: %d", pid);
      callb(n);
    }];
}

void remove_observer(NSNotificationCenter *center, NSObject *obs) {
  [center removeObserver: obs];
}

@implementation WorkspaceWatcher

workspace_callback_t *_callback;

- (id)initWithCallback:(workspace_callback_t *)callback {

  if ((self = [super init])) {

    _callback = callback;

    [[[NSWorkspace sharedWorkspace] notificationCenter] 
      addObserver:self
         selector:@selector(activeDisplayDidChange:)
             name:@"NSWorkspaceActiveDisplayDidChangeNotification"
           object:nil];

    [[[NSWorkspace sharedWorkspace] notificationCenter] 
      addObserver:self
         selector:@selector(activeSpaceDidChange:)
             name:NSWorkspaceActiveSpaceDidChangeNotification
           object:nil];

    [[[NSWorkspace sharedWorkspace] notificationCenter] 
      addObserver:self
         selector:@selector(didActivateApplication:)
             name:NSWorkspaceDidActivateApplicationNotification
           object:nil];

    [[[NSWorkspace sharedWorkspace] notificationCenter] 
      addObserver:self
         selector:@selector(didDeactivateApplication:)
             name:NSWorkspaceDidDeactivateApplicationNotification
           object:nil];

    [[[NSWorkspace sharedWorkspace] notificationCenter] 
      addObserver:self
         selector:@selector(didHideApplication:)
             name:NSWorkspaceDidHideApplicationNotification
           object:nil];

    [[[NSWorkspace sharedWorkspace] notificationCenter] 
      addObserver:self
         selector:@selector(didUnhideApplication:)
             name:NSWorkspaceDidUnhideApplicationNotification
           object:nil];
  }

  return self;
}

- (void)dealloc {
  [[[NSWorkspace sharedWorkspace] notificationCenter] removeObserver:self];
  [super dealloc];
}

- (void)activeDisplayDidChange:(NSNotification *)notification {
  _callback(0, 0);
}

- (void)activeSpaceDidChange:(NSNotification *)notification {
  _callback(1, 0);
}

- (void)didActivateApplication:(NSNotification *)notification {
  pid_t pid = notif_to_pid(notification);
  _callback(2, pid);
}

- (void)didDeactivateApplication:(NSNotification *)notification {
  pid_t pid = notif_to_pid(notification);
  _callback(3, pid);
}

- (void)didHideApplication:(NSNotification *)notification {
  pid_t pid = notif_to_pid(notification);
  _callback(4, pid);
}

- (void)didUnhideApplication:(NSNotification *)notification {
  pid_t pid = notif_to_pid(notification);
  _callback(5, pid);
}

@end

#include <Carbon/Carbon.h>
#include <Cocoa/Cocoa.h>

int processIdentifier_() { return [NSProcessInfo processInfo].processIdentifier; }

NSApplication *nsApplication_sharedApplication() {
  return [NSApplication sharedApplication]; }
NSApplication *nsApp() { return NSApp; }
void app_run(NSApplication *app) { [app run]; }
void app_stop(NSApplication *app, void *p) { [app stop:p]; }
void app_terminate(NSApplication *app, void *p) { [app terminate:p]; }
NSAutoreleasePool *newAutoreleasePool_() { return [NSAutoreleasePool new]; }

NSApplicationActivationPolicy NSApplicationActivationPolicyRegular_() {
  return NSApplicationActivationPolicyRegular;
}
NSApplicationActivationPolicy NSApplicationActivationPolicyAccessory_() {
  return NSApplicationActivationPolicyAccessory;
}
NSApplicationActivationPolicy NSApplicationActivationPolicyProhibited_() {
  return NSApplicationActivationPolicyProhibited;
}

void set_activation_policy(NSApplication *app, NSApplicationActivationPolicy p) {
  [app setActivationPolicy:p];
}

void activate_ignoring_other_apps(NSApplication *app, bool ignoring) {
  [app activateIgnoringOtherApps:ignoring];
}

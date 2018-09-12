#include <Carbon/Carbon.h>
#include <Cocoa/Cocoa.h>

void retainItem(NSStatusItem *item) { [item retain]; }
void releaseItem(NSStatusItem *item) { [item release]; }

NSStatusBar *systemStatusBar_() { return [NSStatusBar systemStatusBar]; }
NSStatusItem *newItem(NSStatusBar *statusBar) {
  return [statusBar statusItemWithLength:NSVariableStatusItemLength];
}
void setItemTitle(NSStatusItem *item, CFStringRef title) {
  [[item button] setTitle:(NSString *)title];
}

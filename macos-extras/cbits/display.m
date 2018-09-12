#include <Cocoa/Cocoa.h>
#include <Carbon/Carbon.h>

CGDirectDisplayID activeDisplay() {
  return [[[[NSScreen mainScreen] deviceDescription]
	    objectForKey:@"NSScreenNumber"] unsignedIntValue]; }

CGDirectDisplayID screen_id(NSScreen *screen) {
  return [[[screen 
             deviceDescription] 
            objectForKey:@"NSScreenNumber"] 
           unsignedIntValue];
}

NSScreen *main_screen() {
  return [NSScreen mainScreen];
}

CFArrayRef screens_ref() {
  return (CFArrayRef)[NSScreen screens];
}

NSScreen *primary_screen() {
  return (NSScreen *)CFArrayGetValueAtIndex(screens_ref(), 0);
}

void screen_full_frame(NSScreen *screen, NSRect *r) {
  NSScreen *primary = primary_screen();
  *r = [screen frame];
  r->origin.y = [primary frame].size.height - r->size.height - r->origin.y;
}

void screen_frame(NSScreen *screen, NSRect *r) {
  NSScreen *primary = primary_screen();
  *r = [screen visibleFrame];
  r->origin.y = [primary frame].size.height - r->size.height - r->origin.y;
}

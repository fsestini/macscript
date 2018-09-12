#include <Cocoa/Cocoa.h>
#include <Carbon/Carbon.h>

void mouse_location(CGPoint *pos) {
  *pos = [NSEvent mouseLocation];
}

unsigned long pressed_mouse_buttons() {
  return [NSEvent pressedMouseButtons];
}

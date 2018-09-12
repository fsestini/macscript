#include <Cocoa/Cocoa.h>

void ns_retain(NSObject *obj) { [obj retain]; }
void ns_release(NSObject *obj) { [obj release]; }

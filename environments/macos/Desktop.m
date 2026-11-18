#import <AppKit/AppKit.h>
int sapiens_update_activate(const char *executable) {
    @autoreleasepool {
        NSString *path = [[NSString stringWithUTF8String:executable] stringByResolvingSymlinksInPath];
        for (NSRunningApplication *app in [[NSWorkspace sharedWorkspace] runningApplications]) {
            if ([app.executableURL.path.stringByResolvingSymlinksInPath isEqualToString:path] && !app.terminated) {
                [app activateWithOptions:NSApplicationActivateAllWindows | NSApplicationActivateIgnoringOtherApps];
                return 1;
            }
        }
        return 0;
    }
}

void sapiens_update_error(const char *message) {
    @autoreleasepool {
        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyAccessory];
        NSAlert *alert = [[NSAlert alloc] init];
        alert.messageText = @"Sapiens4 update could not finish";
        alert.informativeText = [NSString stringWithUTF8String:message];
        [alert addButtonWithTitle:@"OK"];
        [NSApp activateIgnoringOtherApps:YES];
        [alert runModal];
        [alert release];
    }
}

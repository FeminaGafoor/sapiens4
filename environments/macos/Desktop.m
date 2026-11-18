#import <Foundation/Foundation.h>
#include <errno.h>
extern int sapiens_update_publish(const char *, const char *, const void *, size_t, int);

int sapiens_update_preferences(const char *name, const char *path, const char *temporary, int restore) {
    @autoreleasepool {
        NSString *domain = [NSString stringWithUTF8String:name];
        NSUserDefaults *defaults = [[NSUserDefaults alloc] initWithSuiteName:domain];
        int result = 0;
        if (restore) {
            if ([[NSFileManager defaultManager] fileExistsAtPath:[NSString stringWithUTF8String:path]]) {
                NSData *data = [NSData dataWithContentsOfFile:[NSString stringWithUTF8String:path]];
                id value = data ? [NSPropertyListSerialization propertyListWithData:data options:NSPropertyListImmutable format:nil error:nil] : nil;
                if (![value isKindOfClass:[NSDictionary class]]) result = EINVAL;
                else [defaults setPersistentDomain:value forName:domain];
            }
        } else {
            NSDictionary *value = [defaults persistentDomainForName:domain] ?: @{};
            NSData *data = [NSPropertyListSerialization dataWithPropertyList:value format:NSPropertyListBinaryFormat_v1_0 options:0 error:nil];
            result = data ? sapiens_update_publish(path, temporary, data.bytes, data.length, 1) : EINVAL;
        }
        if (restore && ![defaults synchronize] && !result) result = EIO;
        [defaults release];
        return result;
    }
}

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

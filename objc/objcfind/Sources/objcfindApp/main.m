#import <Foundation/Foundation.h>
#import "Finder.h"
#import "FileResult.h"
#import "FileResultFormatter.h"
#import "FindOptions.h"
#import "FindSettings.h"

NSArray* argvToNSArray(int argc, const char * argv[]) {
    NSMutableArray *args = [NSMutableArray array];
    for (int i = 0; i < argc; i++) {
        NSString *arg = [[NSString alloc] initWithCString:argv[i] encoding:NSUTF8StringEncoding];
        [args addObject:arg];
    }
    return [NSArray arrayWithArray:args];
}

void handleError(NSError *error, FindOptions *options, FindSettings *settings) {
    logMsg(@"");
    logErrorColor(error.domain, settings.colorize);
    [options usage:1];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;

        FindOptions *options = [[FindOptions alloc] init];

        NSArray *args = argvToNSArray(argc, argv);

        FindSettings *settings = [options settingsFromArgs:args error:&error];

        if (error) {
            handleError(error, options, settings);
        }

        if (settings.debug) {
            logMsg([NSString stringWithFormat:@"\nsettings: %@", settings]);
        }

        if (settings.printUsage) {
            [options usage:0];
        }

        Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];

        if (error) {
            handleError(error, options, settings);
        }

        NSArray<FileResult*> *fileResults = [finder find:&error];

        if (error) {
            handleError(error, options, settings);
        }
        
        FileResultFormatter *formatter = [[FileResultFormatter alloc] initWithSettings:settings];

        if (settings.printDirs) {
            [finder printMatchingDirs:fileResults formatter:formatter];
        }

        if (settings.printFiles) {
            [finder printMatchingFiles:fileResults formatter:formatter];
        }
    }
    return 0;
}

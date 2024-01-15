#import <Foundation/Foundation.h>
#import "Finder.h"
#import "FileResult.h"
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

void handleError(NSError *error, FindOptions *options) {
    logMsg(@"");
    logError(error.domain);
    [options usage:1];
}

NSArray<NSString*>* getMatchingDirs(NSArray<FileResult*> *fileResults) {
    NSMutableSet<NSString*> *dirSet = [NSMutableSet set];
    for (FileResult *fr in fileResults) {
        [dirSet addObject:[[fr description] stringByDeletingLastPathComponent]];
    }
    NSArray *dirArr = [NSArray arrayWithArray:[dirSet allObjects]];
    return [dirArr sortedArrayUsingComparator:^NSComparisonResult(NSString *s1, NSString *s2) {
        return [s1 compare:s2];
    }];
}

NSArray<NSString*>* getMatchingFiles(NSArray<FileResult*> *fileResults) {
    NSMutableArray<NSString*> *files = [NSMutableArray array];
    for (FileResult *fr in fileResults) {
        [files addObject:[fr description]];
    }
    return [NSArray arrayWithArray:files];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;

        FindOptions *options = [[FindOptions alloc] init];

        NSArray *args = argvToNSArray(argc, argv);

        FindSettings *settings = [options settingsFromArgs:args error:&error];

        if (error) {
            handleError(error, options);
        }

        if (settings.debug) {
            logMsg([NSString stringWithFormat:@"\nsettings: %@", settings]);
        }

        if (settings.printUsage) {
            [options usage:0];
        }

        Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];

        if (error) {
            handleError(error, options);
        }

        NSArray<FileResult*> *fileResults = [finder find:&error];

        if (error) {
            handleError(error, options);
        }

        if (settings.printDirs) {
            NSArray<NSString*> *dirPaths = getMatchingDirs(fileResults);
            if ([dirPaths count] > 0) {
                logMsg([NSString stringWithFormat:@"\nMatching directories (%lu):", [dirPaths count]]);
                for (NSString *d in dirPaths) {
                    logMsg(d);
                }
            } else {
                logMsg(@"\nMatching directories: 0");
            }
        }

        if (settings.printFiles) {
            NSArray<NSString*> *filePaths = getMatchingFiles(fileResults);
            if ([filePaths count] > 0) {
                logMsg([NSString stringWithFormat:@"\nMatching files (%lu):", [filePaths count]]);
                for (NSString *f in filePaths) {
                    logMsg(f);
                }
            } else {
                logMsg(@"\nMatching files: 0");
            }
        }
    }
    return 0;
}

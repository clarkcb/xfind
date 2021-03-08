#import <Foundation/Foundation.h>
#import "Finder.h"
#import "FindFile.h"
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

NSArray<NSString*>* getMatchingDirs(NSArray<FindFile*> *findfiles) {
    NSMutableSet<NSString*> *dirs = [NSMutableSet set];
    for (FindFile *f in findfiles) {
        [dirs addObject:[[f description] stringByDeletingLastPathComponent]];
    }
    NSArray *dirArr = [NSArray arrayWithArray:[dirs allObjects]];
    return [dirArr sortedArrayUsingComparator:^NSComparisonResult(NSString *s1, NSString *s2) {
        return [s1 compare:s2];
    }];
}

NSArray<NSString*>* getMatchingFiles(NSArray<FindFile*> *findfiles) {
    NSMutableSet<NSString*> *files = [NSMutableSet set];
    for (FindFile *f in findfiles) {
        [files addObject:[f description]];
    }
    NSArray *fileArr = [NSArray arrayWithArray:[files allObjects]];
    return [fileArr sortedArrayUsingComparator:^NSComparisonResult(NSString *s1, NSString *s2) {
        return [s1 compare:s2];
    }];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;

        FindOptions *options = [[FindOptions alloc] init];

        NSArray *args = argvToNSArray(argc, argv);
        //for (NSString *arg in args) {
        //    logMsg([NSString stringWithFormat:@"arg: %@", arg]);
        //}

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

        NSArray<FindFile *> *findfiles = [finder find:&error];

        if (error) {
            handleError(error, options);
        }

        if (settings.listDirs) {
            NSArray<NSString*> *dirPaths = getMatchingDirs(findfiles);
            if ([dirPaths count] > 0) {
                logMsg([NSString stringWithFormat:@"\nMatching directories (%lu):", [dirPaths count]]);
                for (NSString *d in dirPaths) {
                    logMsg(d);
                }
            } else {
                logMsg(@"\nMatching directories: 0");
            }
        }

        if (settings.listFiles) {
            NSArray<NSString*> *filePaths = getMatchingFiles(findfiles);
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

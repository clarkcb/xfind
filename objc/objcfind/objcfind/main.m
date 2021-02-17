#import <Foundation/Foundation.h>
#import "Finder.h"
#import "FindOptions.h"
#import "FindResult.h"
#import "FindResultFormatter.h"
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

NSArray<NSString*>* getMatchingDirs(NSArray<FindResult*> *results) {
    NSMutableSet<NSString*> *dirs = [NSMutableSet set];
    for (FindResult *r in results) {
        [dirs addObject:[[[r file] description] stringByDeletingLastPathComponent]];
    }
    NSArray *dirArr = [NSArray arrayWithArray:[dirs allObjects]];
    return [dirArr sortedArrayUsingComparator:^NSComparisonResult(NSString *s1, NSString *s2) {
        return [s1 compare:s2];
    }];
}

NSArray<NSString*>* getMatchingFiles(NSArray<FindResult*> *results) {
    NSMutableSet<NSString*> *files = [NSMutableSet set];
    for (FindResult *r in results) {
        [files addObject:[[r file] description]];
    }
    NSArray *fileArr = [NSArray arrayWithArray:[files allObjects]];
    return [fileArr sortedArrayUsingComparator:^NSComparisonResult(NSString *s1, NSString *s2) {
        return [s1 compare:s2];
    }];
}

NSArray<NSString*>* getMatchingLines(NSArray<FindResult*> *results, FindSettings *settings) {
    NSMutableArray<NSString*> *lines = [NSMutableArray array];
    for (FindResult *r in results) {
        [lines addObject:[[r line] stringByTrimmingCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:@" \t\r\n"]]];
    }
    if (settings.uniqueLines) {
        NSSet<NSString*> *lineSet = [NSSet setWithArray:lines];
        lines = [NSMutableArray arrayWithArray:[lineSet allObjects]];
    }
    return [lines sortedArrayUsingComparator:^NSComparisonResult(NSString *s1, NSString *s2) {
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

        NSArray<FindResult *> *results = [finder find:&error];

        if (error) {
            handleError(error, options);
        }

        if (settings.printResults) {
            FindResultFormatter *formatter = [[FindResultFormatter alloc] initWithSettings:settings];
            logMsg([NSString stringWithFormat:@"\nFind results (%lu):", [results count]]);
            for (FindResult *r in results) {
                logMsg([formatter format:r]);
            }
        }

        if (settings.listDirs) {
            NSArray<NSString*> *dirPaths = getMatchingDirs(results);
            logMsg([NSString stringWithFormat:@"\nDirectories with matches (%lu):", [dirPaths count]]);
            for (NSString *d in dirPaths) {
                logMsg(d);
            }
        }

        if (settings.listFiles) {
            NSArray<NSString*> *filePaths = getMatchingFiles(results);
            logMsg([NSString stringWithFormat:@"\nFiles with matches (%lu):", [filePaths count]]);
            for (NSString *f in filePaths) {
                logMsg(f);
            }
        }

        if (settings.listLines) {
            NSArray<NSString*> *lines = getMatchingLines(results, settings);
            NSString *linesHdr;
            if (settings.uniqueLines) {
                linesHdr = @"Unique lines with matches";
            } else {
                linesHdr = @"Lines with matches";
            }
            logMsg([NSString stringWithFormat:@"\n%@ (%lu):", linesHdr, [lines count]]);
            for (NSString *l in lines) {
                logMsg(l);
            }
        }
    }
    return 0;
}

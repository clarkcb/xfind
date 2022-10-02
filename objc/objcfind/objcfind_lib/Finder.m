#import "common.h"
#import "FileUtil.h"
#import "Finder.h"

@implementation Finder

- (instancetype) initWithSettings:(FindSettings*)settings error:(NSError**)error {
    self = [super init];
    if (self) {
        self.fileTypes = [[FileTypes alloc] init];
        self.settings = settings;
        [self validateSettings:settings error:error];
    }
    return self;
}

- (NSStringEncoding) strToEncoding:(NSString*)s {
    NSStringEncoding encoding =
        CFStringConvertEncodingToNSStringEncoding(CFStringConvertIANACharSetNameToEncoding((CFStringRef) s));
    return encoding;
}

- (void) validateSettings:(FindSettings*)settings error:(NSError**)error {
    if (settings == nil) {
        setError(error, @"Settings not defined");
    } else if ([settings.paths count] == 0) {
        setError(error, @"Startpath not defined");
    } else if (![FileUtil allExist:settings.paths]) {
        setError(error, @"Startpath not found");
    } else if (![FileUtil allReadable:settings.paths]) {
        setError(error, @"Startpath not readable");
    }
}

- (BOOL) matchesAnyPattern:(NSString*)s patterns:(NSArray<Regex*>*)patterns {
    for (Regex *r in patterns) {
        if ([r test:s]) {
            return true;
        }
    }
    return false;
}

- (BOOL) anyMatchesAnyPattern:(NSArray<NSString*>*)ss patterns:(NSArray<Regex*>*)patterns {
    for (NSString *s in ss) {
        if ([self matchesAnyPattern:s patterns:patterns]) {
            return true;
        }
    }
    return false;
}

- (BOOL) filterByExtensions:(NSString*)ext inExtensions:(NSArray<NSString*>*)inExtensions outExtensions:(NSArray<NSString*>*)outExtensions {
    return (([inExtensions count] == 0 || [inExtensions containsObject:ext]) &&
            ([outExtensions count] == 0 || ![outExtensions containsObject:ext]));
}

- (BOOL) filterByPatterns:(NSString*)s inPatterns:(NSArray<Regex*>*)inPatterns outPatterns:(NSArray<Regex*>*)outPatterns {
    return (([inPatterns count] == 0 || [self matchesAnyPattern:s patterns:inPatterns]) &&
            ([outPatterns count] == 0 || ![self matchesAnyPattern:s patterns:outPatterns]));
}

- (BOOL) filterByTypes:(FileType)fileType inTypes:(NSArray<NSNumber*>*)inTypes outTypes:(NSArray<NSNumber*>*)outTypes {
    NSNumber *num = [NSNumber numberWithInt:fileType];
    return (([inTypes count] == 0 || [inTypes containsObject:num]) &&
            ([outTypes count] == 0 || ![outTypes containsObject:num]));
}

- (BOOL) isMatchingDir:(NSString*)dirPath {
    if (self.settings.excludeHidden && [FileUtil isHidden:dirPath]) {
        return false;
    }
    return [self filterByPatterns:dirPath
                       inPatterns:self.settings.inDirPatterns
                      outPatterns:self.settings.outDirPatterns];
}

- (BOOL) isMatchingFile:(NSString*)filePath {
    NSString *fileName = [filePath lastPathComponent];
    return [self filterByExtensions:[FileUtil getExtension:fileName]
                       inExtensions:self.settings.inExtensions
                      outExtensions:self.settings.outExtensions] &&
    [self filterByPatterns:fileName
                inPatterns:self.settings.inFilePatterns
               outPatterns:self.settings.outFilePatterns];
}

- (BOOL) isMatchingArchiveFile:(NSString*)filePath {
    NSString *fileName = [filePath lastPathComponent];
    return [self filterByExtensions:[FileUtil getExtension:fileName]
                       inExtensions:self.settings.inArchiveExtensions
                      outExtensions:self.settings.outArchiveExtensions] &&
    [self filterByPatterns:fileName
                inPatterns:self.settings.inArchiveFilePatterns
               outPatterns:self.settings.outArchiveFilePatterns];
}

- (BOOL) isMatchingFileResult:(FileResult*)fileResult {
    NSString *fileName = [[fileResult filePath] lastPathComponent];
    return [self filterByExtensions:[FileUtil getExtension:fileName]
                       inExtensions:self.settings.inExtensions
                      outExtensions:self.settings.outExtensions] &&
    [self filterByPatterns:fileName
                inPatterns:self.settings.inFilePatterns
               outPatterns:self.settings.outFilePatterns] &&
    [self filterByTypes:[fileResult fileType]
                inTypes:self.settings.inFileTypes
               outTypes:self.settings.outFileTypes];
}

- (FileResult*) filterToFileResult:(NSString*)filePath {
    if (self.settings.excludeHidden && [FileUtil isHidden:filePath]) {
        return false;
    }
    FileType fileType = [self.fileTypes getFileType:[filePath lastPathComponent]];
    FileResult *fr = [[FileResult alloc] initWithFilePath:filePath fileType:fileType];
    if (fileType == FileTypeArchive) {
        if (self.settings.includeArchives && [self isMatchingArchiveFile:filePath]) {
            return fr;
        }
        return nil;
    }
    if (!self.settings.archivesOnly && [self isMatchingFileResult:fr]) {
        return fr;
    }
    return nil;
}

- (NSArray<FileResult*>*) getFileResults:(NSString*)filePath {
    NSMutableArray *fileResults = [NSMutableArray array];
    NSDirectoryEnumerator *enumerator = [FileUtil enumeratorForPath:filePath settings:self.settings];
    NSURL *element = (NSURL*)[enumerator nextObject];
    while (element != nil) {
        NSNumber *isDirectory = nil;
        [element getResourceValue:&isDirectory forKey:NSURLIsDirectoryKey error:nil];
        if ([isDirectory boolValue]) {
            if (![self isMatchingDir:[element path]]) {
                [enumerator skipDescendants];
            }
        } else {
            NSNumber *isRegularFile = nil;
            [element getResourceValue:&isRegularFile forKey:NSURLIsRegularFileKey error:nil];
            if ([isRegularFile boolValue]) {
                NSString *filePath = [element path];
                FileResult *fileResult = [self filterToFileResult:filePath];
                if (fileResult != nil) {
                    [fileResults addObject:fileResult];
                }
            }
        }
        element = (NSURL*)[enumerator nextObject];
    }
    return [[NSArray arrayWithArray:fileResults]
            sortedArrayUsingComparator:^NSComparisonResult(FileResult *sf1, FileResult *sf2) {
        NSString *p1 = [[sf1 description] stringByDeletingLastPathComponent];
        NSString *p2 = [[sf2 description] stringByDeletingLastPathComponent];
        if ([p1 isEqualToString:p2]) {
            NSString *f1 = [[sf1 description] lastPathComponent];
            NSString *f2 = [[sf2 description] lastPathComponent];
            return [f1 compare:f2];
        }
        return [p1 compare:p2];
    }];
}

- (NSArray<FileResult*>*) find:(NSError**)error {
    NSMutableArray<FileResult*> *fileResults = [NSMutableArray array];
    for (NSString *p in self.settings.paths) {
        if ([FileUtil isDirectory:p]) {
            NSArray<FileResult*> *pFiles = [self getFileResults:p];
            [fileResults addObjectsFromArray:pFiles];
        } else {
            FileResult *fr = [self filterToFileResult:p];
            if (fr != nil) {
                [fileResults addObject:fr];
            }
        }
    }

    return [NSArray arrayWithArray:fileResults];
}

@end

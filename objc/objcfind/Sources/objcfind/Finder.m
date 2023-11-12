#import "common.h"
#import "FileUtil.h"
#import "Finder.h"

@implementation Finder

- (instancetype) initWithSettings:(FindSettings*)settings error:(NSError**)error {
    self = [super init];
    if (self) {
        self.fileTypes = [[FileTypes alloc] init];
        self.settings = settings;
        if (![self validateSettings:settings error:error]) {
            return self;
        }
    }
    return self;
}

- (NSStringEncoding) strToEncoding:(NSString*)s {
    NSStringEncoding encoding =
        CFStringConvertEncodingToNSStringEncoding(CFStringConvertIANACharSetNameToEncoding((CFStringRef) s));
    return encoding;
}

- (BOOL) validateSettings:(FindSettings*)settings error:(NSError**)error {
    if (settings == nil) {
        setError(error, @"Settings not defined");
        return false;
    } else if ([settings.paths count] == 0) {
        setError(error, @"Startpath not defined");
        return false;
    } else if (![FileUtil allExist:settings.paths]) {
        setError(error, @"Startpath not found");
        return false;
    } else if (![FileUtil allReadable:settings.paths]) {
        setError(error, @"Startpath not readable");
        return false;
    } else if (settings.maxDepth > -1 && settings.maxDepth < settings.minDepth) {
        setError(error, @"Invalid range for mindepth and maxdepth");
        return false;
    } else if (settings.maxLastMod != nil && settings.minLastMod != nil && [settings.maxLastMod isEqualToDate:[settings.maxLastMod earlierDate:settings.minLastMod]]) {
        setError(error, @"Invalid range for minlastmod and maxlastmod");
        return false;
    } else if (settings.maxSize > 0 && settings.maxSize < settings.minSize) {
        setError(error, @"Invalid range for minsize and maxsize");
        return false;
    }
    return true;
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

- (BOOL) filterByFileTypes:(FileType)fileType inFileTypes:(NSArray<NSNumber*>*)inFileTypes outFileTypes:(NSArray<NSNumber*>*)outFileTypes {
    NSNumber *num = [NSNumber numberWithInt:fileType];
    return (([inFileTypes count] == 0 || [inFileTypes containsObject:num]) &&
            ([outFileTypes count] == 0 || ![outFileTypes containsObject:num]));
}

- (BOOL) filterByStat:(NSDictionary<NSFileAttributeKey, id>*)stat {
    if (stat != nil) {
        if ([self.settings maxLastMod] != nil || [self.settings minLastMod] != nil) {
            NSDate *lastMod = [stat fileModificationDate];
            if (([self.settings maxLastMod] != nil && [lastMod isGreaterThan:[self.settings maxLastMod]]) ||
                ([self.settings minLastMod] != nil && [lastMod isLessThan:[self.settings minLastMod]])) {
                return false;
            }
        }
        if ([self.settings maxSize] > 0 || [self.settings minSize] > 0) {
            NSNumber *fileSize = [[NSNumber alloc] initWithUnsignedLongLong:[stat fileSize]];
            if (([self.settings maxSize] > 0 && [fileSize longValue] > (long)[self.settings maxSize]) ||
                ([self.settings minSize] > 0 && [fileSize longValue] < (long)[self.settings minSize])) {
                return false;
            }
        }
    }
    return true;
}

- (BOOL) isMatchingDir:(NSString*)dirPath {
    if (!self.settings.includeHidden && [FileUtil isHidden:dirPath]) {
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
    BOOL filterByExtensions = true;
    if ([self.settings.inArchiveExtensions count] > 0 || [self.settings.outArchiveExtensions count] > 0) {
        filterByExtensions = [self filterByExtensions:[FileUtil getExtension:fileName]
                                         inExtensions:self.settings.inArchiveExtensions
                                        outExtensions:self.settings.outArchiveExtensions];
    }
    return filterByExtensions &&
    [self filterByPatterns:fileName
                inPatterns:self.settings.inArchiveFilePatterns
               outPatterns:self.settings.outArchiveFilePatterns];
}

- (BOOL) isMatchingFileResult:(FileResult*)fileResult {
    NSString *fileName = [[fileResult filePath] lastPathComponent];
    BOOL filterByExtensions = true;
    if ([self.settings.inExtensions count] > 0 || [self.settings.outExtensions count] > 0) {
        filterByExtensions = [self filterByExtensions:[FileUtil getExtension:fileName]
                                         inExtensions:self.settings.inExtensions
                                        outExtensions:self.settings.outExtensions];
    }
    return filterByExtensions &&
    [self filterByPatterns:fileName
                inPatterns:self.settings.inFilePatterns
               outPatterns:self.settings.outFilePatterns] &&
    [self filterByFileTypes:[fileResult fileType]
                inFileTypes:self.settings.inFileTypes
               outFileTypes:self.settings.outFileTypes] &&
    [self filterByStat:[fileResult stat]];
}

- (FileResult*) filterToFileResult:(NSString*)filePath {
    if (!self.settings.includeHidden && [FileUtil isHidden:filePath]) {
        return false;
    }
    FileType fileType = [self.fileTypes getFileType:[filePath lastPathComponent]];
    NSDictionary<NSFileAttributeKey, id> *stat = nil;
    if ([self.settings needStat]) {
        stat = [[NSFileManager defaultManager] attributesOfItemAtPath:filePath error:nil];
    }
    FileResult *fr = [[FileResult alloc] initWithFilePath:filePath fileType:fileType stat:stat];
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
            if ((self.settings.maxDepth > 0 && enumerator.level > self.settings.maxDepth) || ![self isMatchingDir:[element path]]) {
                [enumerator skipDescendants];
            }
        } else {
            NSNumber *isRegularFile = nil;
            [element getResourceValue:&isRegularFile forKey:NSURLIsRegularFileKey error:nil];
            if ([isRegularFile boolValue]) {
                if ((self.settings.minDepth < 0 || enumerator.level >= self.settings.minDepth) && (self.settings.maxDepth < 1 || enumerator.level <= self.settings.maxDepth)) {
                    NSString *filePath = [element path];
                    FileResult *fileResult = [self filterToFileResult:filePath];
                    if (fileResult != nil) {
                        [fileResults addObject:fileResult];
                    }
                }
            }
        }
        element = (NSURL*)[enumerator nextObject];
    }
    return [NSArray arrayWithArray:fileResults];
}

- (NSArray<FileResult*>*) sortFileResults:(NSArray<FileResult*>*)fileResults {
    NSMutableArray<FileResult*> *sortedFileResults;
    if (self.settings.sortBy == SortByFileName) {
        sortedFileResults = [NSMutableArray arrayWithArray:[fileResults sortedArrayUsingComparator:^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
            return [fr1 compareByName:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
        }]];
    } else if (self.settings.sortBy == SortByFileSize) {
        sortedFileResults = [NSMutableArray arrayWithArray:[fileResults sortedArrayUsingComparator:^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
            return [fr1 compareBySize:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
        }]];
    } else if (self.settings.sortBy == SortByFileType) {
        sortedFileResults = [NSMutableArray arrayWithArray:[fileResults sortedArrayUsingComparator:^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
            return [fr1 compareByType:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
        }]];
    } else if (self.settings.sortBy == SortByLastMod) {
        sortedFileResults = [NSMutableArray arrayWithArray:[fileResults sortedArrayUsingComparator:^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
            return [fr1 compareByLastMod:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
        }]];
    } else {
        sortedFileResults = [NSMutableArray arrayWithArray:[fileResults sortedArrayUsingComparator:^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
            return [fr1 compareByPath:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
        }]];
    }
    if (self.settings.sortDescending) {
        sortedFileResults = [NSMutableArray arrayWithArray:[[sortedFileResults reverseObjectEnumerator] allObjects]];
    }
    return [NSArray arrayWithArray:sortedFileResults];
}

- (NSArray<FileResult*>*) find:(NSError**)error {
    NSMutableArray<FileResult*> *fileResults = [NSMutableArray array];
    for (NSString *p in self.settings.paths) {
        if ([FileUtil isDirectory:p]) {
            // if maxDepth is zero, we can skip since a directory cannot be a result
            if (self.settings.maxDepth != 0) {
                NSArray<FileResult*> *pFiles = [self getFileResults:p];
                [fileResults addObjectsFromArray:pFiles];
            }
        } else {
            // if minDepth > zero, we can skip since the file is at depth zero
            if (self.settings.minDepth <= 0) {
                FileResult *fr = [self filterToFileResult:p];
                if (fr != nil) {
                    [fileResults addObject:fr];
                }
            }
        }
    }

    return [self sortFileResults:[NSArray arrayWithArray:fileResults]];
}

@end

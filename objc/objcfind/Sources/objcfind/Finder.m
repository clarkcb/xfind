#import "common.h"
#import "FileResultSorter.h"
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
        setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_DEFINED]);
        return false;
    } else if (![FileUtil allExist:settings.paths]) {
        setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_FOUND]);
        return false;
    } else if (![FileUtil allReadable:settings.paths]) {
        setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_READABLE]);
        return false;
    } else if (settings.maxDepth > -1 && settings.maxDepth < settings.minDepth) {
        setError(error, [NSString stringWithUTF8String:INVALID_RANGE_MINDEPTH_MAXDEPTH]);
        return false;
    } else if (settings.maxLastMod != nil && settings.minLastMod != nil && [settings.maxLastMod isEqualToDate:[settings.maxLastMod earlierDate:settings.minLastMod]]) {
        setError(error, [NSString stringWithUTF8String:INVALID_RANGE_MINLASTMOD_MAXLASTMOD]);
        return false;
    } else if (settings.maxSize > 0 && settings.maxSize < settings.minSize) {
        setError(error, [NSString stringWithUTF8String:INVALID_RANGE_MINSIZE_MAXSIZE]);
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

- (BOOL) filterByLastMod:(NSDate*)lastMod {
    if ([self.settings maxLastMod] != nil || [self.settings minLastMod] != nil) {
        return (([self.settings maxLastMod] == nil || [lastMod isLessThanOrEqualTo:[self.settings maxLastMod]]) &&
            ([self.settings minLastMod] == nil || [lastMod isGreaterThanOrEqualTo:[self.settings minLastMod]]));
    }
    return true;
}

- (BOOL) filterByFileSize:(unsigned long long)fileSize {
    if ([self.settings maxSize] > 0 || [self.settings minSize] > 0) {
        NSNumber *numFileSize = [[NSNumber alloc] initWithUnsignedLongLong:fileSize];
        return (([self.settings maxSize] == 0 || [numFileSize longValue] <= (long)[self.settings maxSize]) &&
         ([self.settings minSize] == 0 || [numFileSize longValue] >= (long)[self.settings minSize]));
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
    [self filterByFileSize:[fileResult fileSize]] &&
    [self filterByLastMod:[fileResult lastMod]];
}

- (FileResult*) filterToFileResult:(NSString*)filePath error:(NSError**)error {
    if (!self.settings.includeHidden && [FileUtil isHidden:filePath]) {
        return false;
    }
    FileType fileType = [self.fileTypes getFileType:[filePath lastPathComponent]];
    if (fileType == FileTypeArchive && ![self.settings includeArchives] && ![self.settings archivesOnly]) {
        return nil;
    }
    unsigned long long fileSize = 0;
    NSDate *lastMod = nil;
    if ([self.settings needSize] || [self.settings needLastMod]) {
        NSDictionary<NSFileAttributeKey, id> *stat = [FileUtil getFileAttributes:filePath error:error];
        if (*error) {
            return nil;
        }
        if ([self.settings needSize]) fileSize = stat.fileSize;
        if ([self.settings needLastMod]) lastMod = stat.fileModificationDate;
    }
    FileResult *fr = [[FileResult alloc] initWithFilePath:filePath fileType:fileType fileSize:fileSize lastMod:lastMod];
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

- (NSArray<FileResult*>*) recGetFileResults:(NSString*)dirPath minDepth:(long)minDepth maxDepth:(long)maxDepth currentDepth:(long)currentDepth error:(NSError**)error {
    NSMutableArray *fileResults = [NSMutableArray array];
    BOOL recurse = true;
    if (currentDepth == maxDepth) {
        recurse = false;
    } else if (maxDepth > -1 && currentDepth > maxDepth) {
        return fileResults;
    }
    
    NSArray<NSString*>* pathElems = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:dirPath
                                                                             error:error];
    if (*error) {
        return nil;
    }

    NSMutableArray *pathDirs = [NSMutableArray array];
    for (NSString *pathElem in pathElems) {
        NSString* path = [FileUtil joinPath:dirPath childPath:pathElem];
        BOOL linkIsDir = false;
        BOOL linkIsFile = false;
        if ([FileUtil isSymlink:path]) {
            if (self.settings.followSymlinks) {
                NSString* targetPath = [FileUtil getSymlinkTarget:path];
                if (targetPath != nil) {
                    if ([FileUtil isDirectory:targetPath]) {
                        linkIsDir = true;
                    } else if ([FileUtil isReadableFile:targetPath]) {
                        linkIsFile = true;
                    }
                }
            } else {
                continue;
            }
        }
        if ([FileUtil isDirectory:path] || linkIsDir) {
            if (recurse && [self isMatchingDir:pathElem]) {
                [pathDirs addObject:path];
            }
        } else if (([FileUtil isReadableFile:path] || linkIsFile) && (minDepth < 0 || currentDepth >= minDepth)) {
            FileResult *fileResult = [self filterToFileResult:path error:error];
            if (*error) {
                return nil;
            }
            if (fileResult != nil) {
                [fileResults addObject:fileResult];
            }
        }
    }
    
    for (NSString *pathDir in pathDirs) {
        NSArray<FileResult*> *pathResults = [self recGetFileResults:pathDir minDepth:minDepth maxDepth:maxDepth currentDepth:(currentDepth + 1) error:error];
        if (*error) {
            return nil;
        }
        [fileResults addObjectsFromArray:pathResults];
    }

    return [NSArray arrayWithArray:fileResults];
}

- (NSArray<FileResult*>*) getFileResults:(NSString*)filePath error:(NSError**)error {
    NSString* fp = filePath;
    if (![FileUtil exists:filePath]) {
        fp = [FileUtil expandPath:filePath];
    }
    NSMutableArray *fileResults = [NSMutableArray array];
    if ([FileUtil isDirectory:fp]) {
        // if maxDepth is zero, we can skip since a directory cannot be a result
        if (self.settings.maxDepth == 0) {
            return fileResults;
        }
        if ([self isMatchingDir:fp]) {
            long maxDepth = self.settings.recursive ? self.settings.maxDepth : 1;
            return [self recGetFileResults:fp minDepth:self.settings.minDepth maxDepth:maxDepth currentDepth:1 error:error];
        } else {
            setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_MATCH_FIND_SETTINGS]);
            return nil;
        }
    } else {
        // if minDepth > zero, we can skip since the file is at depth zero
        if (self.settings.minDepth > 0) {
            return fileResults;
        }
        FileResult *fileResult = [self filterToFileResult:fp error:error];
        if (*error) {
            return nil;
        }
        if (fileResult != nil) {
            [fileResults addObject:fileResult];
        } else {
            setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_MATCH_FIND_SETTINGS]);
            return nil;
        }
    }

    return [NSArray arrayWithArray:fileResults];
}

- (NSArray<FileResult*>*) find:(NSError**)error {
    NSMutableArray<FileResult*> *fileResults = [NSMutableArray array];
    for (NSString *p in self.settings.paths) {
        NSArray<FileResult*> *pathResults = [self getFileResults:p error:error];
        if (*error) {
            return nil;
        }
        [fileResults addObjectsFromArray:pathResults];
    }

    FileResultSorter *fileResultSorter = [[FileResultSorter alloc] initWithSettings:self.settings];
    return [fileResultSorter sort:[NSArray arrayWithArray:fileResults]];
}

- (NSArray<NSString*>*) getMatchingDirs:(NSArray<FileResult*>*)fileResults {
    NSMutableSet<NSString*> *dirSet = [NSMutableSet set];
    for (FileResult *fr in fileResults) {
        [dirSet addObject:[[fr description] stringByDeletingLastPathComponent]];
    }
    NSArray *dirArr = [NSArray arrayWithArray:[dirSet allObjects]];
    return [dirArr sortedArrayUsingComparator:^NSComparisonResult(NSString *s1, NSString *s2) {
        return [s1 compare:s2];
    }];
}

- (void) printMatchingDirs:(NSArray<FileResult*>*)fileResults formatter:(FileResultFormatter*)formatter {
    NSArray<NSString*> *dirs = [self getMatchingDirs:fileResults];
    if ([dirs count] > 0) {
        logMsg([NSString stringWithFormat:@"\nMatching directories (%lu):", [dirs count]]);
        for (NSString *d in dirs) {
            logMsg(formatter.formatDirPath(d));
        }
    } else {
        logMsg(@"\nMatching directories: 0");
    }
}

- (void) printMatchingFiles:(NSArray<FileResult*>*)fileResults formatter:(FileResultFormatter*)formatter {
    if ([fileResults count] > 0) {
        logMsg([NSString stringWithFormat:@"\nMatching files (%lu):", [fileResults count]]);
        for (FileResult *fr in fileResults) {
            logMsg([formatter formatFileResult:fr]);
        }
    } else {
        logMsg(@"\nMatching files: 0");
    }
}

@end

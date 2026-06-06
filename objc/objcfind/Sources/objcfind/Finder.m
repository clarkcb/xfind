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
    } else if (settings.paths == nil || [settings.paths count] == 0) {
        setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_DEFINED]);
        return false;
    }
    for (NSString *path in settings.paths) {
        NSString *p = path;
        if (![FileUtil exists:p]) {
            p = [FileUtil expandPath:p];
            if (![FileUtil exists:p]) {
                setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_FOUND]);
                return false;
            }
        }
        if (![FileUtil isReadableFile:p]) {
            setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_READABLE]);
            return false;
        }
        NSString* resolvedPath = p;
        if ([FileUtil isSymlink:p]) {
            if (settings.followSymlinks) {
                resolvedPath = [FileUtil getSymlinkTarget:path];
            } else {
                setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_MATCH_FIND_SETTINGS]);
                return false;
            }
        }
        if ([FileUtil isDirectory:resolvedPath]) {
            // still check p and not resolvedPath because p is the name entered
            if (![self isTraversableDirPath:p]) {
                setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_MATCH_FIND_SETTINGS]);
                return false;
            }
        } else if ([FileUtil isReadableFile:resolvedPath]) {
            // still check p and not resolvedPath because p is the name entered
            FileResult *fr = [self filterToFileResult:p error:error];
            if (*error) {
                return false;
            }
            if (fr == nil) {
                setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_MATCH_FIND_SETTINGS]);
                return false;
            }
        }
    }
    if (settings.maxDepth > -1 && settings.maxDepth < settings.minDepth) {
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

- (BOOL) emptyOrMatchesAnyPattern:(NSString*)s patterns:(NSArray<Regex*>*)patterns {
    return [patterns count] == 0 || [self matchesAnyPattern:s patterns:patterns];
}

- (BOOL) emptyOrNotMatchesAnyPattern:(NSString*)s patterns:(NSArray<Regex*>*)patterns {
    return [patterns count] == 0 || ![self matchesAnyPattern:s patterns:patterns];
}

- (BOOL) emptyOrAnyMatchesAnyPattern:(NSArray<NSString*>*)ss patterns:(NSArray<Regex*>*)patterns {
    return [patterns count] == 0 || [self anyMatchesAnyPattern:ss patterns:patterns];
}

- (BOOL) emptyOrNotAnyMatchesAnyPattern:(NSArray<NSString*>*)ss patterns:(NSArray<Regex*>*)patterns {
    return [patterns count] == 0 || ![self anyMatchesAnyPattern:ss patterns:patterns];
}

- (BOOL) emptyOrMatchesAnyString:(NSString*)s strings:(NSArray<NSString*>*)strings {
    return [strings count] == 0 || [strings containsObject:s];
}

- (BOOL) emptyOrNotMatchesAnyString:(NSString*)s strings:(NSArray<NSString*>*)strings {
    return [strings count] == 0 || ![strings containsObject:s];
}

- (BOOL) emptyOrMatchesAnyFileType:(FileType)fileType fileTypes:(NSArray<NSNumber*>*)fileTypes {
    if ([fileTypes count] > 0) {
        NSNumber *num = [NSNumber numberWithInt:fileType];
        return [fileTypes containsObject:num];
    }
    return true;
}

- (BOOL) emptyOrNotMatchesAnyFileType:(FileType)fileType fileTypes:(NSArray<NSNumber*>*)fileTypes {
    if ([fileTypes count] > 0) {
        NSNumber *num = [NSNumber numberWithInt:fileType];
        return ![fileTypes containsObject:num];
    }
    return true;
}

- (BOOL) isMatchingDirPathByHidden:(NSString*)dirPath {
    return self.settings.includeHidden || ![FileUtil isHiddenPath:dirPath];
}

- (BOOL) isMatchingDirPathByInPatterns:(NSString*)dirPath {
    return [self emptyOrMatchesAnyPattern:dirPath patterns:self.settings.inDirPatterns];
}

- (BOOL) isMatchingDirPathByOutPatterns:(NSString*)dirPath {
    return [self emptyOrNotMatchesAnyPattern:dirPath patterns:self.settings.outDirPatterns];
}

- (BOOL) isTraversableDirPath:(NSString*)dirPath {
    return [self isMatchingDirPathByHidden:dirPath]
        && [self isMatchingDirPathByOutPatterns:dirPath];
}

- (BOOL) isMatchingDirPath:(NSString*)dirPath {
    return [self isMatchingDirPathByHidden:dirPath]
        && [self isMatchingDirPathByInPatterns:dirPath]
        && [self isMatchingDirPathByOutPatterns:dirPath];
}

- (BOOL) isNullOrMatchingDirPath:(NSString*)dirPath {
    return dirPath == nil
        || ([self isMatchingDirPathByHidden:dirPath]
        && [self isMatchingDirPathByInPatterns:dirPath]
        && [self isMatchingDirPathByOutPatterns:dirPath]);
}

- (BOOL) isMatchingFileNameByHidden:(NSString*)fileName {
    return self.settings.includeHidden || ![FileUtil isHiddenName:fileName];
}

- (BOOL) isMatchingArchiveExtension:(NSString*)ext {
    return [self emptyOrMatchesAnyString:ext strings:self.settings.inArchiveExtensions]
        && [self emptyOrNotMatchesAnyString:ext strings:self.settings.outArchiveExtensions];
}

- (BOOL) isMatchingArchiveExtensionForFilePath:(NSString*)filePath {
    if ([self.settings.inArchiveExtensions count] > 0 || [self.settings.outArchiveExtensions count] > 0) {
        NSString *fileName = [filePath lastPathComponent];
        NSString *ext = [FileUtil getExtension:fileName];
        return [self isMatchingArchiveExtension:ext];
    }
    return true;
}

- (BOOL) isMatchingArchiveFileName:(NSString*)fileName {
    return [self emptyOrMatchesAnyPattern:fileName patterns:self.settings.inArchiveFilePatterns]
        && [self emptyOrNotMatchesAnyPattern:fileName patterns:self.settings.outArchiveFilePatterns];
}

- (BOOL) isMatchingArchiveFileNameForFilePath:(NSString*)filePath {
    if ([self.settings.inArchiveFilePatterns count] > 0 || [self.settings.outArchiveFilePatterns count] > 0) {
        NSString *fileName = [filePath lastPathComponent];
        return [self isMatchingArchiveFileName:fileName];
    }
    return true;
}

- (BOOL) isMatchingArchiveFilePath:(NSString*)filePath {
    return [self isMatchingArchiveExtensionForFilePath:filePath]
        && [self isMatchingArchiveFileNameForFilePath:filePath];
}

- (BOOL) isMatchingArchiveFileResult:(FileResult*)fileResult {
    return [self isMatchingArchiveFilePath:[fileResult filePath]];
}


- (BOOL) isMatchingExtension:(NSString*)ext {
    return [self emptyOrMatchesAnyString:ext strings:self.settings.inExtensions]
        && [self emptyOrNotMatchesAnyString:ext strings:self.settings.outExtensions];
}

- (BOOL) isMatchingExtensionForFilePath:(NSString*)filePath {
    if ([self.settings.inExtensions count] > 0 || [self.settings.outExtensions count] > 0) {
        NSString *fileName = [filePath lastPathComponent];
        NSString *ext = [FileUtil getExtension:fileName];
        return [self isMatchingExtension:ext];
    }
    return true;
}

- (BOOL) isMatchingFileName:(NSString*)fileName {
    return [self emptyOrMatchesAnyPattern:fileName patterns:self.settings.inFilePatterns]
        && [self emptyOrNotMatchesAnyPattern:fileName patterns:self.settings.outFilePatterns];
}

- (BOOL) isMatchingFileNameForFilePath:(NSString*)filePath {
    if ([self.settings.inFilePatterns count] > 0 || [self.settings.outFilePatterns count] > 0) {
        NSString *fileName = [filePath lastPathComponent];
        return [self isMatchingFileName:fileName];
    }
    return true;
}

- (BOOL) isMatchingFilePath:(NSString*)filePath {
    return [self isMatchingExtensionForFilePath:filePath]
        && [self isMatchingFileNameForFilePath:filePath];
}

- (BOOL) isMatchingFileType:(FileType)fileType {
    return [self emptyOrMatchesAnyFileType:fileType fileTypes:[self.settings inFileTypes]]
        && [self emptyOrNotMatchesAnyFileType:fileType fileTypes:[self.settings outFileTypes]];
}

- (BOOL) isMatchingFileSize:(unsigned long long)fileSize {
    if ([self.settings maxSize] > 0 || [self.settings minSize] > 0) {
        NSNumber *numFileSize = [[NSNumber alloc] initWithUnsignedLongLong:fileSize];
        return ([self.settings maxSize] == 0 || [numFileSize longValue] <= (long)[self.settings maxSize])
            && ([self.settings minSize] == 0 || [numFileSize longValue] >= (long)[self.settings minSize]);
    }
    return true;
}

- (BOOL) isMatchingLastMod:(NSDate*)lastMod {
    if ([self.settings maxLastMod] != nil || [self.settings minLastMod] != nil) {
        return lastMod != nil
            && ([self.settings maxLastMod] == nil || [lastMod isLessThanOrEqualTo:[self.settings maxLastMod]])
            && ([self.settings minLastMod] == nil || [lastMod isGreaterThanOrEqualTo:[self.settings minLastMod]]);
    }
    return true;
}

- (BOOL) isMatchingFileResult:(FileResult*)fileResult {
    return [self isMatchingFilePath:[fileResult filePath]]
        && [self isMatchingFileType:[fileResult fileType]]
        && [self isMatchingFileSize:[fileResult fileSize]]
        && [self isMatchingLastMod:[fileResult lastMod]];
}

- (FileResult*) filterArchiveFilePathToFileResult:(NSString*)filePath error:(NSError**)error {
    if (!self.settings.includeArchives && !self.settings.archivesOnly) {
        return nil;
    }
    
    if (![self isMatchingArchiveFilePath:filePath]) {
        return nil;
    }

    unsigned long long fileSize = 0;
    NSDate *lastMod = nil;
    return [[FileResult alloc] initWithFilePath:filePath fileType:FileTypeArchive fileSize:fileSize lastMod:lastMod];
}

- (FileResult*) filterRegularFilePathToFileResult:(NSString*)filePath fileType:(FileType)fileType error:(NSError**)error {
    if (self.settings.archivesOnly) {
        return nil;
    }

    if (![self isMatchingFilePath:filePath] || ![self isMatchingFileType:fileType]) {
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

        if (![self isMatchingFileSize:fileSize] || ![self isMatchingLastMod:lastMod]) {
            return nil;
        }
    }

    return [[FileResult alloc] initWithFilePath:filePath fileType:fileType fileSize:fileSize lastMod:lastMod];
}

- (FileResult*) filterToFileResult:(NSString*)filePath error:(NSError**)error {
    NSString *parent = [filePath stringByDeletingLastPathComponent];
    if (![self isNullOrMatchingDirPath:parent]) {
        return nil;
    }

    NSString *fileName = [filePath lastPathComponent];
    if (![self isMatchingFileNameByHidden:fileName]) {
        return nil;
    }

    FileType fileType = [self.fileTypes getFileType:fileName];
    if (fileType == FileTypeArchive) {
        return [self filterArchiveFilePathToFileResult:filePath error:error];
    }

    return [self filterRegularFilePathToFileResult:filePath fileType:fileType error:error];
}

- (NSArray<FileResult*>*) recGetFileResults:(NSString*)dirPath minDepth:(long)minDepth maxDepth:(long)maxDepth currentDepth:(long)currentDepth error:(NSError**)error {
    NSMutableArray *fileResults = [NSMutableArray array];
    BOOL recurse = true;
    if (currentDepth == maxDepth) {
        recurse = false;
    } else if (maxDepth > -1 && currentDepth > maxDepth) {
        return fileResults;
    }
    
    NSArray<NSString*>* dirElems = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:dirPath
                                                                             error:error];
    if (*error) {
        return nil;
    }

    NSMutableArray *subDirs = [NSMutableArray array];
    for (NSString *dirElem in dirElems) {
        NSString* subPath = [FileUtil joinPath:dirPath childPath:dirElem];
        BOOL linkIsDir = false;
        BOOL linkIsFile = false;
        if ([FileUtil isSymlink:subPath]) {
            if (self.settings.followSymlinks) {
                NSString* targetPath = [FileUtil getSymlinkTarget:subPath];
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
        if ([FileUtil isDirectory:subPath] || linkIsDir) {
            if (recurse && [self isTraversableDirPath:dirElem]) {
                [subDirs addObject:subPath];
            }
        } else if (([FileUtil isReadableFile:subPath] || linkIsFile) && (minDepth < 0 || currentDepth >= minDepth)) {
            FileResult *fileResult = [self filterToFileResult:subPath error:error];
            if (*error) {
                return nil;
            }
            if (fileResult != nil) {
                [fileResults addObject:fileResult];
            }
        }
    }
    
    for (NSString *subDir in subDirs) {
        NSArray<FileResult*> *dirResults = [self recGetFileResults:subDir minDepth:minDepth maxDepth:maxDepth currentDepth:(currentDepth + 1) error:error];
        if (*error) {
            return nil;
        }
        [fileResults addObjectsFromArray:dirResults];
    }

    return [NSArray arrayWithArray:fileResults];
}

- (NSArray<FileResult*>*) getFileResults:(NSString*)filePath error:(NSError**)error {
    NSString* fp = filePath;
    if (![FileUtil exists:filePath]) {
        fp = [FileUtil expandPath:filePath];
        if (![FileUtil exists:fp]) {
            setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_FOUND]);
            return nil;
        }
    }
    NSMutableArray *fileResults = [NSMutableArray array];
    BOOL linkIsDir = false;
    BOOL linkIsFile = false;
    if ([FileUtil isSymlink:fp]) {
        if (self.settings.followSymlinks) {
            NSString* targetPath = [FileUtil getSymlinkTarget:fp];
            if (targetPath != nil) {
                if ([FileUtil isDirectory:targetPath]) {
                    linkIsDir = true;
                } else if ([FileUtil isReadableFile:targetPath]) {
                    linkIsFile = true;
                }
            }
        } else {
            setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_MATCH_FIND_SETTINGS]);
            return nil;
        }
    }
    if ([FileUtil isDirectory:fp] || linkIsDir) {
        // if maxDepth is zero, we can skip since a directory cannot be a result
        if (self.settings.maxDepth == 0) {
            return fileResults;
        }
        if ([self isTraversableDirPath:fp]) {
            long maxDepth = self.settings.recursive ? self.settings.maxDepth : 1;
            return [self recGetFileResults:fp minDepth:self.settings.minDepth maxDepth:maxDepth currentDepth:1 error:error];
        } else {
            setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_MATCH_FIND_SETTINGS]);
            return nil;
        }
    } else if ([FileUtil isReadableFile:fp] || linkIsFile) {
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
    } else {
        setError(error, [NSString stringWithUTF8String:STARTPATH_NOT_MATCH_FIND_SETTINGS]);
        return nil;
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

    if ([fileResults count] > 1) {
        FileResultSorter *fileResultSorter = [[FileResultSorter alloc] initWithSettings:self.settings];
        return [fileResultSorter sort:[NSArray arrayWithArray:fileResults]];
    }
    return fileResults;
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

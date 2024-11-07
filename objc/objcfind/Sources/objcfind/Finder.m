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

- (FileResult*) filterToFileResult:(NSString*)filePath {
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
        NSDictionary<NSFileAttributeKey, id> *stat = [[NSFileManager defaultManager] attributesOfItemAtPath:filePath error:nil];
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
            FileResult *fileResult = [self filterToFileResult:path];
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
    NSMutableArray *fileResults = [NSMutableArray array];
    if ([FileUtil isDirectory:filePath]) {
        // if maxDepth is zero, we can skip since a directory cannot be a result
        if (self.settings.maxDepth == 0) {
            return fileResults;
        }
        if ([self isMatchingDir:filePath]) {
            long maxDepth = self.settings.recursive ? self.settings.maxDepth : 1;
            return [self recGetFileResults:filePath minDepth:self.settings.minDepth maxDepth:maxDepth currentDepth:1 error:error];
        } else {
            setError(error, @"Startpath does not match find settings");
            return nil;
        }
    } else {
        // if minDepth > zero, we can skip since the file is at depth zero
        if (self.settings.minDepth > 0) {
            return fileResults;
        }
        FileResult *fileResult = [self filterToFileResult:filePath];
        if (fileResult != nil) {
            [fileResults addObject:fileResult];
        } else {
            setError(error, @"Startpath does not match find settings");
            return nil;
        }
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
        NSArray<FileResult*> *pathResults = [self getFileResults:p error:error];
        if (*error) {
            return nil;
        }
        [fileResults addObjectsFromArray:pathResults];
    }

    return [self sortFileResults:[NSArray arrayWithArray:fileResults]];
}

@end

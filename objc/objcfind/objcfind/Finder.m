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
    } else if (settings.startPath == nil || [settings.startPath length] == 0) {
        setError(error, @"Startpath not defined");
    } else if (![FileUtil exists:settings.startPath] && ![FileUtil exists:[FileUtil expandPath:settings.startPath]]) {
        setError(error, @"Startpath not found");
    } else if (![FileUtil isReadableFile:settings.startPath] && ![FileUtil isReadableFile:[FileUtil expandPath:settings.startPath]]) {
        setError(error, @"Startpath not readable");
    } else if ([settings.findPatterns count] == 0) {
        setError(error, @"No find patterns defined");
    } else if (settings.linesAfter < 0) {
        setError(error, @"Invalid linesafter");
    } else if (settings.linesBefore < 0) {
        setError(error, @"Invalid linesbefore");
    } else if (settings.maxLineLength < 0) {
        setError(error, @"Invalid maxlinelength");
    } else {
        NSStringEncoding encoding = [self strToEncoding:settings.textFileEncoding];
        if (encoding == 0xFFFFFFFF) {
            setError(error, @"Invalid textfileencoding");
        } else {
            self.textFileEncoding = encoding;
        }
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

- (BOOL) isFindDir:(NSString*)dirPath {
    if ([FileUtil isHidden:dirPath] && self.settings.excludeHidden) {
        return false;
    }
    return [self filterByPatterns:dirPath inPatterns:self.settings.inDirPatterns outPatterns:self.settings.outDirPatterns];
}

- (BOOL) isFindFile:(NSString*)filePath {
    return [self filterByExtensions:[FileUtil getExtension:filePath]
                       inExtensions:self.settings.inExtensions
                      outExtensions:self.settings.outExtensions] &&
    [self filterByPatterns:filePath
                inPatterns:self.settings.inFilePatterns
               outPatterns:self.settings.outFilePatterns];
}

- (BOOL) isFindFindFile:(FindFile*)findFile {
    return [self filterByExtensions:[FileUtil getExtension:[findFile filePath]]
                       inExtensions:self.settings.inExtensions
                      outExtensions:self.settings.outExtensions] &&
    [self filterByPatterns:[findFile filePath]
                inPatterns:self.settings.inFilePatterns
               outPatterns:self.settings.outFilePatterns] &&
    [self filterByTypes:[findFile fileType]
                inTypes:self.settings.inFileTypes
               outTypes:self.settings.outFileTypes]
;
}

- (BOOL) isArchiveFindFile:(NSString*)filePath {
    return [self filterByExtensions:[FileUtil getExtension:filePath]
                       inExtensions:self.settings.inArchiveExtensions
                      outExtensions:self.settings.outArchiveExtensions] &&
    [self filterByPatterns:filePath
                inPatterns:self.settings.inArchiveFilePatterns
               outPatterns:self.settings.outArchiveFilePatterns];
}

- (BOOL) filterFile:(NSString*)filePath {
    if ([FileUtil isHidden:filePath] && self.settings.excludeHidden) {
        return false;
    }
    FileType fileType = [self.fileTypes getFileType:filePath];
    if (fileType == FileTypeArchive) {
        return self.settings.findArchives && [self isArchiveFindFile:filePath];
    }
    return !self.settings.archivesOnly && [self isFindFile:filePath];
}

- (FindFile*) filterToFindFile:(NSString*)filePath {
    if ([FileUtil isHidden:filePath] && self.settings.excludeHidden) {
        return false;
    }
    FileType fileType = [self.fileTypes getFileType:filePath];
    if (fileType == FileTypeUnknown) {
        return nil;
    }
    FindFile *sf = [[FindFile alloc] initWithFilePath:filePath fileType:fileType];
    if ((fileType == FileTypeArchive && self.settings.findArchives && [self isArchiveFindFile:filePath]) ||
        (!self.settings.archivesOnly && [self isFindFindFile:sf])) {
        return sf;
    }
    return nil;
}

- (NSArray<FindResult*>*) find:(NSError**)error {
    //logMsg(@"Finding...");
    NSMutableArray<FindResult*> *results = [NSMutableArray array];
    if ([FileUtil isDirectory:self.settings.startPath]) {
        [results addObjectsFromArray:[self findDirPath:self.settings.startPath error:error]];
    } else {
        // TODO: looks like we need to do filterFile on this
        FileType fileType = [self.fileTypes getFileType:self.settings.startPath];
        FindFile *sf = [[FindFile alloc]
                                initWithFilePath:self.settings.startPath
                                fileType:fileType];
        [results addObjectsFromArray:[self findFile:sf error:error]];
    }
    return [NSArray arrayWithArray:results];
}

- (NSArray<FindResult*>*) findDirPath:(NSString*)filePath error:(NSError**)error {
    //logMsg(@"Finding path...");
    NSArray<FindFile*> *findFiles = [self getFindFiles:filePath];

    if (self.settings.verbose) {
        NSMutableSet<NSString*> *dirSet = [NSMutableSet set];
        for (FindFile *sf in findFiles) {
            [dirSet addObject:[[sf description] stringByDeletingLastPathComponent]];
        }
        NSArray *findDirs = [[NSArray arrayWithArray:[dirSet allObjects]] sortedArrayUsingComparator:^NSComparisonResult(NSString *s1, NSString *s2) {
            return [s1 compare:s2];
        }];

        logMsg([NSString stringWithFormat:@"\nDirectories to be found (%lu):", [findDirs count]]);
        for (NSString *d in findDirs) {
            logMsg([FileUtil relativePath:d to:self.settings.startPath]);
        }

        logMsg([NSString stringWithFormat:@"\nFiles to be found (%lu):", [findFiles count]]);
        for (FindFile *sf in findFiles) {
            logMsg([FileUtil relativePath:[sf description] to:self.settings.startPath]);
        }
    }

    NSMutableArray<FindResult*> *results = [NSMutableArray array];
    for (FindFile *sf in findFiles) {
        [results addObjectsFromArray:[self findFile:sf error:error]];
        if (*error != nil) {
            return [NSArray array];
        }
    }
    return [NSArray arrayWithArray:results];
}

- (NSArray<FindFile*>*) getFindFiles:(NSString*)filePath {
    NSMutableArray *findFiles = [NSMutableArray array];
    NSDirectoryEnumerator *enumerator = [FileUtil enumeratorForPath:filePath settings:self.settings];
    NSURL *element = (NSURL*)[enumerator nextObject];
    while (element != nil) {
        NSNumber *isDirectory = nil;
        [element getResourceValue:&isDirectory forKey:NSURLIsDirectoryKey error:nil];
        if ([isDirectory boolValue]) {
            if (![self isFindDir:[element path]]) {
                [enumerator skipDescendants];
            }
        } else {
            NSNumber *isRegularFile = nil;
            [element getResourceValue:&isRegularFile forKey:NSURLIsRegularFileKey error:nil];
            if ([isRegularFile boolValue]) {
                NSString *filePath = [element path];
                FindFile *findFile = [self filterToFindFile:filePath];
                if (findFile != nil) {
                    [findFiles addObject:findFile];
                }
            }
        }
        element = (NSURL*)[enumerator nextObject];
    }
    return [[NSArray arrayWithArray:findFiles] sortedArrayUsingComparator:^NSComparisonResult(FindFile *sf1, FindFile *sf2) {
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


- (NSArray<FindResult*>*) findFilePath:(NSString*)filePath error:(NSError**)error {
    FileType fileType = [self.fileTypes getFileType:filePath];
    FindFile *sf = [[FindFile alloc] initWithFilePath:filePath fileType:fileType];
    return [self findFile:sf error:error];
}

- (NSArray<FindResult*>*) findFile:(FindFile*)sf error:(NSError**)error {
    switch (sf.fileType) {
        case FileTypeCode:
        case FileTypeText:
        case FileTypeXml:
            return [self findTextFile:sf error:error];
            break;
        case FileTypeBinary:
            return [self findBinaryFile:sf error:error];
            break;
        case FileTypeArchive:
        default:
            return [NSArray array];
            break;
    }
}

- (NSArray<FindResult*>*) findBinaryFile:(FindFile*)sf error:(NSError**)error {
    NSString *contents = [[NSString alloc] initWithContentsOfFile:sf.filePath
                                                         encoding:NSISOLatin1StringEncoding
                                                            error:error];
    if (*error != nil) {
        return [NSArray array];
    }
    NSMutableArray<FindResult*> *results = [NSMutableArray array];
    if (contents != nil) {
        for (Regex *p in self.settings.findPatterns) {
            NSArray<NSTextCheckingResult*> *matches = [p matches:contents];
            if ([matches count] > 0 && self.settings.firstMatch) {
                matches = [NSArray arrayWithObject:matches[0]];
            }
            for (NSTextCheckingResult *m in matches) {
                FindResult *r = [[FindResult alloc] initWithPattern:p.pattern
                                                                   file:sf
                                                                lineNum:0
                                                        matchStartIndex:m.range.location + 1
                                                          matchEndIndex:m.range.location + m.range.length + 1
                                                                   line:@""
                                                            linesBefore:[NSArray array]
                                                             linesAfter:[NSArray array]];
                [results addObject:r];
            }
        }
    }
    return results;
}

- (NSArray<FindResult*>*) findTextFile:(FindFile*)sf error:(NSError**)error {
    NSArray<FindResult*> *results = [NSArray array];
    if (self.settings.multiLineFind) {
        results = [self findTextFileContents:sf error:error];
    } else {
        // there is no line reader in ObjC, might not bother
        // implementing line-based finding
        //[self findTextFileLines:sf error:error];
        results = [self findTextFileContents:sf error:error];
    }
    return results;
}

- (NSArray<FindResult*>*) findTextFileContents:(FindFile*)sf error:(NSError**)error {
    NSString *contents = [[NSString alloc] initWithContentsOfFile:sf.filePath
                                                         encoding:self.textFileEncoding
                                                            error:error];
    if (*error != nil) {
        if ([[*error domain] isEqualToString:@"NSCocoaErrorDomain"] && [*error code] == 261) {
            // this indicates problem reading text file with encoding, just reset the error and move on
            *error = nil;
        } else {
            return [NSArray array];
        }
    }
    NSArray<FindResult*> *results = [NSArray array];
    if (contents != nil) {
        results = [self findMultiLineString:contents error:error];
        for (FindResult *r in results) {
            r.file = sf;
        }
    }
    return results;
}

- (NSArray<FindResult*>*) findMultiLineString:(NSString*)s error:(NSError**)error {
    NSMutableArray<FindResult*> *results = [NSMutableArray array];
    for (Regex *p in self.settings.findPatterns) {
        [results addObjectsFromArray:[self findMultiLineString:s pattern:p error:error]];
    }
    return [NSArray arrayWithArray:results];
}



- (NSArray<NSString*>*) getLinesAfter:(NSString*)s
                      beforeLineCount:(int)beforeLineCount
                     startLineIndices:(NSArray<NSNumber*>*)startLineIndices
                       endLineIndices:(NSArray<NSNumber*>*)endLineIndices {
    NSMutableArray *linesAfter = [NSMutableArray array];
    
    if (self.settings.linesAfter > 0) {
        int afterLineCount = (int)[startLineIndices count] - beforeLineCount - 1;
        int getLineCount = MIN(self.settings.linesAfter, afterLineCount);
        for (int i=1; i <= getLineCount; i++) {
            long startLineIndex = [startLineIndices[beforeLineCount + i] longValue];
            long endLineIndex = [endLineIndices[beforeLineCount + i] longValue];
            NSString *lineAfter = [self lineFromIndices:s
                                         startLineIndex:startLineIndex
                                           endLineIndex:endLineIndex];
            [linesAfter addObject:lineAfter];
        }
    }
    
    return [NSArray arrayWithArray:linesAfter];
}

- (NSArray<FindResult*>*) findMultiLineString:(NSString*)s
                                          pattern:(Regex*)pattern
                                            error:(NSError**)error {
    NSMutableArray<FindResult*> *results = [NSMutableArray array];
    NSArray<NSNumber*> *newLineIndices = [self getNewLineIndices:s];
    NSArray<NSNumber*> *startLineIndices = [self getStartLineIndices:newLineIndices];
    NSArray<NSNumber*> *endLineIndices = [self getEndLineIndices:newLineIndices lastIndex:[s length]];
    NSArray<NSTextCheckingResult*> *matches;
    if (self.settings.firstMatch) {
        NSTextCheckingResult *match = [pattern firstMatch:s];
        if (match != nil) {
            matches = [NSArray arrayWithObject:match];
        } else {
            matches = [NSArray array];
        }
    } else {
        matches = [pattern matches:s];
    }
    for (NSTextCheckingResult *m in matches) {
        int beforeLineCount = [self countLessThan:m.range.location array:startLineIndices] - 1;
        if (beforeLineCount < 0) {
            beforeLineCount = 0;
        }
        NSMutableArray *linesBefore = [NSMutableArray array];
        if (self.settings.linesBefore > 0) {
            int getLineCount = MIN(self.settings.linesBefore, beforeLineCount);
            for (int i=getLineCount; i > 0; i--) {
                long startLineIndex = [startLineIndices[beforeLineCount - i] longValue];
                long endLineIndex = [endLineIndices[beforeLineCount - i] longValue];
                NSString *lineBefore = [self lineFromIndices:s
                                              startLineIndex:startLineIndex
                                                endLineIndex:endLineIndex];
                [linesBefore addObject:lineBefore];
            }
        }

        NSArray *linesAfter = [NSArray array];
        if (self.settings.linesAfter > 0) {
            linesAfter = [self getLinesAfter:s
                             beforeLineCount:beforeLineCount
                            startLineIndices:startLineIndices
                              endLineIndices:endLineIndices];
        }

        if (([linesBefore count] == 0 || [self linesBeforeMatch:linesBefore]) &&
            ([linesAfter count] == 0 || [self linesAfterMatch:linesAfter])) {
            long startLineIndex = [startLineIndices[beforeLineCount] longValue];
            long endLineIndex = [endLineIndices[beforeLineCount] longValue];
            NSString *line = [self lineFromIndices:s startLineIndex:startLineIndex endLineIndex:endLineIndex];
            long lineNum = beforeLineCount + 1;
            long matchStartIndex = m.range.location - startLineIndex + 1;
            long matchEndIndex = m.range.location + m.range.length - startLineIndex + 1;
            FindResult *r = [[FindResult alloc] initWithPattern:pattern.pattern
                                                               file:nil
                                                            lineNum:lineNum
                                                    matchStartIndex:matchStartIndex
                                                      matchEndIndex:matchEndIndex
                                                               line:line
                                                        linesBefore:[NSArray arrayWithArray:linesBefore]
                                                         linesAfter:linesAfter];
            [results addObject:r];
        }
    }
    return [NSArray arrayWithArray:results];
}

- (BOOL) linesMatch:(NSArray<NSString*>*)lines
         inPatterns:(NSArray<Regex*>*)inPatterns
        outPatterns:(NSArray<Regex*>*)outPatterns {
    return (([inPatterns count] == 0 || [self anyMatchesAnyPattern:lines patterns:inPatterns]) &&
            ([outPatterns count] == 0 || ![self anyMatchesAnyPattern:lines patterns:outPatterns]));
}

- (BOOL) linesBeforeMatch:(NSArray<NSString*>*)linesBefore {
    return [self linesMatch:linesBefore
                 inPatterns:self.settings.inLinesBeforePatterns
                outPatterns:self.settings.outLinesBeforePatterns];
}

- (BOOL) linesAfterMatch:(NSArray<NSString*>*)linesAfter {
    return [self linesMatch:linesAfter
                 inPatterns:self.settings.inLinesAfterPatterns
                outPatterns:self.settings.outLinesAfterPatterns];
}

- (NSArray<NSNumber*>*) getNewLineIndices:(NSString*)s {
    NSMutableArray<NSNumber*> *newLineIndices = [NSMutableArray array];
    for (long i=0; i < [s length]; i++) {
        unichar c = [s characterAtIndex:i];
        if (c == '\n') {
            [newLineIndices addObject:[NSNumber numberWithLong:i]];
        }
    }
    return [NSArray arrayWithArray:newLineIndices];
}

- (NSArray<NSNumber*>*) getStartLineIndices:(NSArray<NSNumber*>*)newLineIndices {
    NSMutableArray<NSNumber*> *startLineIndices = [NSMutableArray arrayWithObject:[NSNumber numberWithLong:0]];
    for (NSNumber *n in newLineIndices) {
        [startLineIndices addObject:[NSNumber numberWithInteger:[n longValue] + 1]];
    }
    return [NSArray arrayWithArray:startLineIndices];
}

- (NSArray<NSNumber*>*) getEndLineIndices:(NSArray<NSNumber*>*)newLineIndices lastIndex:(long)lastIndex {
    NSMutableArray<NSNumber*> *endLineIndices = [NSMutableArray arrayWithArray:newLineIndices];
    [endLineIndices addObject:[NSNumber numberWithLong:lastIndex]];
    return [NSArray arrayWithArray:endLineIndices];
}

- (int) countLessThan:(long)num array:(NSArray<NSNumber*>*)array {
    int count = 0;
    while ( count < [array count] && [array[count] longValue] < num) {
        count++;
    }
    return count;
}

- (NSString*) lineFromIndices:(NSString*)s
               startLineIndex:(long)startLineIndex
                 endLineIndex:(long)endLineIndex {
    NSString *line = [s substringWithRange:NSMakeRange(startLineIndex, endLineIndex - startLineIndex)];
    return line;
}

@end

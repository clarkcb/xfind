#import "common.h"
#import "FileTypes.h"
#import "FindSettings.h"

@implementation FindSettings

@synthesize archivesOnly = _archivesOnly;
@synthesize debug = _debug;

- (instancetype) init {
    self = [super init];
    if (self) {
        self.archivesOnly = false;
        self.colorize = true;
        self.debug = false;
        self.dirColor = ColorCyan;
        self.extColor = ColorYellow;
        self.fileColor = ColorMagenta;
        self.followSymlinks = false;
        self.includeArchives = false;
        self.includeHidden = false;
        self.printDirs = false;
        self.printFiles = false;
        self.printUsage = false;
        self.printVersion = false;
        self.recursive = true;
        self.sortCaseInsensitive = false;
        self.sortDescending = false;
        self.verbose = false;

        self.maxDepth = -1;
        self.maxLastMod = nil;
        self.maxSize = 0;
        self.minDepth = -1;
        self.minLastMod = nil;
        self.minSize = 0;

        self.inArchiveExtensions = [[NSMutableArray alloc] init];
        self.inArchiveFilePatterns = [[NSMutableArray alloc] init];
        self.inDirPatterns = [[NSMutableArray alloc] init];
        self.inExtensions = [[NSMutableArray alloc] init];
        self.inFilePatterns = [[NSMutableArray alloc] init];
        self.inFileTypes = [[NSMutableArray alloc] init];
        self.outArchiveExtensions = [[NSMutableArray alloc] init];
        self.outArchiveFilePatterns = [[NSMutableArray alloc] init];
        self.outDirPatterns = [[NSMutableArray alloc] init];
        self.outExtensions = [[NSMutableArray alloc] init];
        self.outFilePatterns = [[NSMutableArray alloc] init];
        self.outFileTypes = [[NSMutableArray alloc] init];
        self.paths = [[NSMutableArray alloc] init];
        self.sortBy = SortByFilePath;
    }
    return self;
}

- (NSString *) description {
    NSMutableString *d = [[NSMutableString alloc] initWithString:@"FindSettings("];
    [d appendFormat:@"archivesOnly=%@", boolToNSString(self.archivesOnly)];
    [d appendFormat:@", colorize=%@", boolToNSString(self.colorize)];
    [d appendFormat:@", debug=%@", boolToNSString(self.debug)];
    [d appendFormat:@", followSymlinks=%@", boolToNSString(self.followSymlinks)];
    [d appendFormat:@", inArchiveExtensions=%@", arrayToNSString(self.inArchiveExtensions)];
    [d appendFormat:@", inArchiveFilePatterns=%@", arrayToNSString(self.inArchiveFilePatterns)];
    [d appendFormat:@", inDirPatterns=%@", arrayToNSString(self.inDirPatterns)];
    [d appendFormat:@", inExtensions=%@", arrayToNSString(self.inExtensions)];
    [d appendFormat:@", inFilePatterns=%@", arrayToNSString(self.inFilePatterns)];
    [d appendFormat:@", inFileTypes=%@", [FindSettings fileTypesArrayToNSString:self.inFileTypes]];
    [d appendFormat:@", includeArchives=%@", boolToNSString(self.includeArchives)];
    [d appendFormat:@", includeHidden=%@", boolToNSString(self.includeHidden)];
    [d appendFormat:@", maxDepth=%ld", (long)self.maxDepth];
    [d appendFormat:@", maxLastMod=%@", [FindSettings lastModToNSString:self.maxLastMod]];
    [d appendFormat:@", maxSize=%lu", (long)self.maxSize];
    [d appendFormat:@", minDepth=%ld", (long)self.minDepth];
    [d appendFormat:@", minLastMod=%@", [FindSettings lastModToNSString:self.minLastMod]];
    [d appendFormat:@", minSize=%lu", (long)self.minSize];
    [d appendFormat:@", outArchiveExtensions=%@", arrayToNSString(self.outArchiveExtensions)];
    [d appendFormat:@", outArchiveFilePatterns=%@", arrayToNSString(self.outArchiveFilePatterns)];
    [d appendFormat:@", outDirPatterns=%@", arrayToNSString(self.outDirPatterns)];
    [d appendFormat:@", outExtensions=%@", arrayToNSString(self.outExtensions)];
    [d appendFormat:@", outFilePatterns=%@", arrayToNSString(self.outFilePatterns)];
    [d appendFormat:@", outFileTypes=%@", [FindSettings fileTypesArrayToNSString:self.outFileTypes]];
    [d appendFormat:@", paths=%@", arrayToNSString(self.paths)];
    [d appendFormat:@", printDirs=%@", boolToNSString(self.printDirs)];
    [d appendFormat:@", printFiles=%@", boolToNSString(self.printFiles)];
    [d appendFormat:@", printUsage=%@", boolToNSString(self.printUsage)];
    [d appendFormat:@", printVersion=%@", boolToNSString(self.printVersion)];
    [d appendFormat:@", recursive=%@", boolToNSString(self.recursive)];
    [d appendFormat:@", sortBy=%@", [FindSettings getNameFromSortBy:self.sortBy]];
    [d appendFormat:@", sortCaseInsensitive=%@", boolToNSString(self.sortCaseInsensitive)];
    [d appendFormat:@", sortDescending=%@", boolToNSString(self.sortDescending)];
    [d appendFormat:@", verbose=%@", boolToNSString(self.verbose)];
    [d appendString:@")"];
    return d;
}

- (void) addExtensions:(NSString *)ext toArr:(NSMutableArray *)arr {
    NSArray *exts = [ext componentsSeparatedByString:@","];
    [arr addObjectsFromArray:exts];
}

- (void) addPattern:(NSString *)pattern toArr:(NSMutableArray *)arr {
    [arr addObject:[[Regex alloc] initWithPattern:pattern]];
}

- (void) addInArchiveExtension:(NSString *)ext {
    [self addExtensions:ext toArr:self.inArchiveExtensions];
}

- (void) addInArchiveFilePattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.inArchiveFilePatterns];
}

- (void) addInDirPattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.inDirPatterns];
}

- (void) addInExtension:(NSString *)ext {
    [self addExtensions:ext toArr:self.inExtensions];
}

- (void) addInFilePattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.inFilePatterns];
}

- (void) addOutArchiveExtension:(NSString *)ext {
    [self addExtensions:ext toArr:self.outArchiveExtensions];
}

- (void) addOutArchiveFilePattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.outArchiveFilePatterns];
}

- (void) addOutDirPattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.outDirPatterns];
}

- (void) addOutExtension:(NSString *)ext {
    [self addExtensions:ext toArr:self.outExtensions];
}

- (void) addOutFilePattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.outFilePatterns];
}

- (void) addFileType:(NSString *)typeName toArr:(NSMutableArray *)arr {
    FileType fileType = [FileTypes fromName:typeName];
    [arr addObject:[NSNumber numberWithInt:fileType]];
    // TODO: if we do this for objc/swift we need to do it for all of other languages
    // if fileType is FileTypeText, add text sub-types
//    if (fileType == FileTypeText) {
//        [arr addObject:[NSNumber numberWithInt:FileTypeCode]];
//        [arr addObject:[NSNumber numberWithInt:FileTypeXml]];
//    }
}

- (void) addInFileType:(NSString *)typeName {
    [self addFileType:typeName toArr:self.inFileTypes];
}

- (void) addOutFileType:(NSString *)typeName {
    [self addFileType:typeName toArr:self.outFileTypes];
}

- (void) addPath:(NSString *)path {
    [self.paths addObject:path];
}

- (BOOL) archivesOnly {
    return _archivesOnly;
}

- (void)setArchivesOnly:(BOOL)b {
    _archivesOnly = b;
    if (b) {
        [self setIncludeArchives:b];
    }
}

- (BOOL) debug {
    return _debug;
}

- (void)setDebug:(BOOL)b {
    _debug = b;
    if (b) {
        [self setVerbose:b];
    }
}

- (void) setMaxDepthFromString:(NSString*)depthStr {
    [self setMaxDepth:[depthStr intValue]];
}

- (void) setMaxLastModFromString:(NSString*)dateStr {
    [self setMaxLastMod:stringToNSDate(dateStr)];
}

- (void) setMaxSizeFromString:(NSString*)sizeStr {
    [self setMaxSize:[sizeStr intValue]];
}

- (void) setMinDepthFromString:(NSString*)depthStr {
    [self setMinDepth:[depthStr intValue]];
}

- (void) setMinLastModFromString:(NSString*)dateStr {
    [self setMinLastMod:stringToNSDate(dateStr)];
}

- (void) setMinSizeFromString:(NSString*)sizeStr {
    [self setMinSize:[sizeStr intValue]];
}

+ (SortBy)getSortByFromName:(NSString *)sortByName {
    NSString *lname = [sortByName lowercaseString];
    if (lname == [NSString stringWithUTF8String:S_FILENAME] || lname == [NSString stringWithUTF8String:S_NAME]) {
        return SortByFileName;
    }
    if (lname == [NSString stringWithUTF8String:S_FILESIZE] || lname == [NSString stringWithUTF8String:S_SIZE]) {
        return SortByFileSize;
    }
    if (lname == [NSString stringWithUTF8String:S_FILETYPE] || lname == [NSString stringWithUTF8String:S_TYPE]) {
        return SortByFileType;
    }
    if (lname == [NSString stringWithUTF8String:S_LASTMOD]) {
        return SortByLastMod;
    }
    return SortByFilePath;
}

+ (NSString*) getNameFromSortBy:(SortBy)sortBy {
    if (sortBy == SortByFileName) {
        return [NSString stringWithUTF8String:S_FILENAME];
    }
    if (sortBy == SortByFileSize) {
        return [NSString stringWithUTF8String:S_FILESIZE];
    }
    if (sortBy == SortByFileType) {
        return [NSString stringWithUTF8String:S_FILETYPE];
    }
    if (sortBy == SortByLastMod) {
        return [NSString stringWithUTF8String:S_LASTMOD];
    }
    return [NSString stringWithUTF8String:S_FILEPATH];
}

- (void) setSortByFromName:(NSString*)sortByName {
    self.sortBy = [FindSettings getSortByFromName:sortByName];

}

+ (NSString*) lastModToNSString:(NSDate *)lastMod {
    if (lastMod == nil) {
        return @"0";
    }
    return dateToNSString(lastMod);
}

+ (NSString*) fileTypesArrayToNSString:(NSArray<NSNumber*>*)arr {
    NSMutableString *arrString = [NSMutableString stringWithString:@"["];
    for (int i=0; i < [arr count]; i++) {
        if (i > 0) {
            [arrString appendString:@", "];
        }
        NSString *typeName = [FileTypes toName:[arr[i] intValue]];
        [arrString appendFormat:@"%@", typeName];
    }
    [arrString appendString:@"]"];
    return [NSString stringWithString:arrString];
}

- (BOOL) needLastMod {
    return self.sortBy == SortByLastMod ||
    self.maxLastMod != nil || self.minLastMod != nil;
}

- (BOOL) needSize {
    return self.sortBy == SortByFileSize ||
    self.maxSize > 0 || self.minSize > 0;
}

@end

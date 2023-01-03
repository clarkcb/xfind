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
        self.debug = false;
        self.excludeHidden = true;
        self.includeArchives = false;
        self.listDirs = false;
        self.listFiles = false;
        self.printUsage = false;
        self.printVersion = false;
        self.recursive = true;
        self.sortDescending = false;
        self.verbose = false;

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
    [d appendFormat:@", debug=%@", boolToNSString(self.debug)];
    [d appendFormat:@", excludeHidden=%@", boolToNSString(self.excludeHidden)];
    [d appendFormat:@", inArchiveExtensions=%@", arrayToNSString(self.inArchiveExtensions)];
    [d appendFormat:@", inArchiveFilePatterns=%@", arrayToNSString(self.inArchiveFilePatterns)];
    [d appendFormat:@", inDirPatterns=%@", arrayToNSString(self.inDirPatterns)];
    [d appendFormat:@", inExtensions=%@", arrayToNSString(self.inExtensions)];
    [d appendFormat:@", inFilePatterns=%@", arrayToNSString(self.inFilePatterns)];
    [d appendFormat:@", inFileTypes=%@", fileTypesArrayToNSString(self.inFileTypes)];
    [d appendFormat:@", includeArchives=%@", boolToNSString(self.includeArchives)];
    [d appendFormat:@", listDirs=%@", boolToNSString(self.listDirs)];
    [d appendFormat:@", listFiles=%@", boolToNSString(self.listFiles)];
    [d appendFormat:@", outArchiveExtensions=%@", arrayToNSString(self.outArchiveExtensions)];
    [d appendFormat:@", outArchiveFilePatterns=%@", arrayToNSString(self.outArchiveFilePatterns)];
    [d appendFormat:@", outDirPatterns=%@", arrayToNSString(self.outDirPatterns)];
    [d appendFormat:@", outExtensions=%@", arrayToNSString(self.outExtensions)];
    [d appendFormat:@", outFilePatterns=%@", arrayToNSString(self.outFilePatterns)];
    [d appendFormat:@", outFileTypes=%@", fileTypesArrayToNSString(self.outFileTypes)];
    [d appendFormat:@", paths=%@", arrayToNSString(self.paths)];
    [d appendFormat:@", printUsage=%@", boolToNSString(self.printUsage)];
    [d appendFormat:@", printVersion=%@", boolToNSString(self.printVersion)];
    [d appendFormat:@", recursive=%@", boolToNSString(self.recursive)];
    [d appendFormat:@", sortBy=%@", [FindSettings getNameFromSortBy:self.sortBy]];
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
    // if fileType is FileTypeText, add text sub-types
    if (fileType == FileTypeText) {
        [arr addObject:[NSNumber numberWithInt:FileTypeCode]];
        [arr addObject:[NSNumber numberWithInt:FileTypeXml]];
    }
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

+ (SortBy)getSortByFromName:(NSString *)sortByName {
    NSString *lname = [sortByName lowercaseString];
    if (lname == [NSString stringWithUTF8String:S_FILENAME]) {
        return SortByFileName;
    }
    if (lname == [NSString stringWithUTF8String:S_FILETYPE]) {
        return SortByFileType;
    }
    return SortByFilePath;
}

+ (NSString*) getNameFromSortBy:(SortBy)sortBy {
    if (sortBy == SortByFileName) {
        return [NSString stringWithUTF8String:S_FILENAME];
    }
    if (sortBy == SortByFileType) {
        return [NSString stringWithUTF8String:S_FILETYPE];
    }
    return [NSString stringWithUTF8String:S_FILEPATH];
}

- (void) setSortByFromName:(NSString*)sortByName {
    self.sortBy = [FindSettings getSortByFromName:sortByName];

}

NSString* fileTypesArrayToNSString(NSArray<NSNumber*> *arr) {
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

@end

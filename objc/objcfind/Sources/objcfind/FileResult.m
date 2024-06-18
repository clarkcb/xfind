#import "FileResult.h"
#import "FileTypes.h"

@implementation FileResult

- (instancetype) initWithFilePath:(NSString *)filePath fileType:(FileType)fileType fileSize:(unsigned long long)fileSize lastMod:(NSDate*)lastMod {
    self = [super init];
    if (self) {
        self.containers = [NSArray array];
        self.filePath = filePath;
        self.fileType = fileType;
        self.fileSize = fileSize;
        self.lastMod = lastMod;
    }
    return self;
}

- (NSString *)description {
    NSMutableString *s = [NSMutableString string];
    if ([self.containers count] > 0) {
        [s appendFormat:@"%@!", [self.containers componentsJoinedByString:@"!"]];
    }
    [s appendString:self.filePath];
    return [NSString stringWithString:s];
}

- (NSComparisonResult)compareByPath:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive {
    NSString *thisPath = [NSString stringWithString:[self.filePath stringByDeletingLastPathComponent]];
    NSString *otherPath = [NSString stringWithString:[otherFileResult.filePath stringByDeletingLastPathComponent]];
    if (caseInsensitive) {
        thisPath = [thisPath lowercaseString];
        otherPath = [otherPath lowercaseString];
    }
    if ([thisPath isEqualToString:otherPath]) {
        NSString *thisFileName = [NSString stringWithString:[self.filePath lastPathComponent]];
        NSString *otherFileName = [NSString stringWithString:[otherFileResult.filePath lastPathComponent]];
        if (caseInsensitive) {
            thisFileName = [thisFileName lowercaseString];
            otherFileName = [otherFileName lowercaseString];
        }
        return [thisFileName compare:otherFileName];
    }
    return [thisPath compare:otherPath];
}

- (NSComparisonResult)compareByName:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive {
    NSString *thisFileName = [NSString stringWithString:[self.filePath lastPathComponent]];
    NSString *otherFileName = [NSString stringWithString:[otherFileResult.filePath lastPathComponent]];
    if (caseInsensitive) {
        thisFileName = [thisFileName lowercaseString];
        otherFileName = [otherFileName lowercaseString];
    }
    if ([thisFileName isEqualToString:otherFileName]) {
        NSString *thisPath = [NSString stringWithString:[self.filePath stringByDeletingLastPathComponent]];
        NSString *otherPath = [NSString stringWithString:[otherFileResult.filePath stringByDeletingLastPathComponent]];
        if (caseInsensitive) {
            thisPath = [thisPath lowercaseString];
            otherPath = [otherPath lowercaseString];
        }
        return [thisPath compare:otherPath];
    }
    return [thisFileName compare:otherFileName];
}

- (NSComparisonResult)compareBySize:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive {
    NSNumber *thisFileSize = [[NSNumber alloc] initWithUnsignedLongLong:self.fileSize];
    NSNumber *otherFileSize = [[NSNumber alloc] initWithUnsignedLongLong:otherFileResult.fileSize];
    if ([thisFileSize isEqualToNumber:otherFileSize]) {
        return [self compareByPath:otherFileResult caseInsensitive:caseInsensitive];
    }
    return [thisFileSize compare:otherFileSize];
}

- (NSComparisonResult)compareByType:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive {
    NSString *thisFileType = [FileTypes toName:self.fileType];
    NSString *otherFileType = [FileTypes toName:otherFileResult.fileType];
    if ([thisFileType isEqualToString:otherFileType]) {
        return [self compareByPath:otherFileResult caseInsensitive:caseInsensitive];
    }
    return [thisFileType compare:otherFileType];
}

- (NSComparisonResult)compareByLastMod:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive {
    if ([self.lastMod isEqualToDate:[otherFileResult lastMod]]) {
        return [self compareByPath:otherFileResult caseInsensitive:caseInsensitive];
    }
    return [self.lastMod compare:[otherFileResult lastMod]];
}

@end

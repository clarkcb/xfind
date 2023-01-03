#import "FileResult.h"
#import "FileTypes.h"

@implementation FileResult

- (instancetype) initWithFilePath:(NSString *)filePath fileType:(FileType)fileType {
    self = [super init];
    if (self) {
        self.containers = [NSArray array];
        self.filePath = filePath;
        self.fileType = fileType;
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

- (NSComparisonResult)compareByPath:(FileResult *)otherFileResult {
    NSString *thisPath = [NSString stringWithString:[self.filePath stringByDeletingLastPathComponent]];
    NSString *otherPath = [NSString stringWithString:[otherFileResult.filePath stringByDeletingLastPathComponent]];
    if ([thisPath isEqualToString:otherPath]) {
        NSString *thisFileName = [NSString stringWithString:[self.filePath lastPathComponent]];
        NSString *otherFileName = [NSString stringWithString:[otherFileResult.filePath lastPathComponent]];
        return [thisFileName compare:otherFileName];
    }
    return [thisPath compare:otherPath];
}

- (NSComparisonResult)compareByName:(FileResult *)otherFileResult {
    NSString *thisFileName = [NSString stringWithString:[self.filePath lastPathComponent]];
    NSString *otherFileName = [NSString stringWithString:[otherFileResult.filePath lastPathComponent]];
    if ([thisFileName isEqualToString:otherFileName]) {
        NSString *thisPath = [NSString stringWithString:[self.filePath stringByDeletingLastPathComponent]];
        NSString *otherPath = [NSString stringWithString:[otherFileResult.filePath stringByDeletingLastPathComponent]];
        return [thisPath compare:otherPath];
    }
    return [thisFileName compare:otherFileName];
}

- (NSComparisonResult)compareByType:(FileResult *)otherFileResult {
    NSString *thisFileType = [FileTypes toName:self.fileType];
    NSString *otherFileType = [FileTypes toName:otherFileResult.fileType];
    if ([thisFileType isEqualToString:otherFileType]) {
        return [self compareByPath:otherFileResult];
    }
    return [thisFileType compare:otherFileType];
}

@end

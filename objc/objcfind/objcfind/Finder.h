#import <Foundation/Foundation.h>
#import "FileTypes.h"
#import "FindFile.h"
#import "FindSettings.h"

@interface Finder : NSObject

@property FileTypes *fileTypes;
@property FindSettings *settings;
@property NSStringEncoding textFileEncoding;

- (instancetype) initWithSettings:(FindSettings*)settings error:(NSError**)error;
- (NSArray<FindFile*>*) find:(NSError**)error;

// private methods
- (FindFile*) filterToFindFile:(NSString*)filePath;
- (BOOL) isArchiveFindFile:(NSString*)filePath;
- (BOOL) isFindDir:(NSString*)dirPath;
- (BOOL) isFindFile:(NSString*)filePath;

@end

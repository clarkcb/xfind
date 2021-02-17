#import <Foundation/Foundation.h>
#import "FileTypes.h"
#import "FindFile.h"
#import "FindResult.h"
#import "FindSettings.h"

@interface Finder : NSObject

@property FileTypes *fileTypes;
@property FindSettings *settings;
@property NSStringEncoding textFileEncoding;

- (instancetype) initWithSettings:(FindSettings*)settings error:(NSError**)error;
- (NSArray<FindResult*>*) find:(NSError**)error;
- (NSArray<FindResult*>*) findDirPath:(NSString*)filePath error:(NSError**)error;
- (NSArray<FindResult*>*) findFilePath:(NSString*)filePath error:(NSError**)error;
- (NSArray<FindResult*>*) findFile:(FindFile*)sf error:(NSError**)error;
- (NSArray<FindResult*>*) findMultiLineString:(NSString*)s error:(NSError**)error;

// private methods
- (BOOL) filterFile:(NSString*)filePath;
- (BOOL) isArchiveFindFile:(NSString*)filePath;
- (BOOL) isFindDir:(NSString*)dirPath;
- (BOOL) isFindFile:(NSString*)filePath;

@end

#ifndef Finder_h
#define Finder_h

#import <Foundation/Foundation.h>
#import "FileTypes.h"
#import "FileResult.h"
#import "FindSettings.h"

@interface Finder : NSObject

@property FileTypes *fileTypes;
@property FindSettings *settings;
@property NSStringEncoding textFileEncoding;

- (instancetype) initWithSettings:(FindSettings*)settings error:(NSError**)error;
- (NSArray<FileResult*>*) find:(NSError**)error;

// private methods
- (FileResult*) filterToFileResult:(NSString*)filePath;
- (BOOL) isMatchingArchiveFile:(NSString*)filePath;
- (BOOL) isMatchingDir:(NSString*)dirPath;
- (BOOL) isMatchingFile:(NSString*)filePath;
- (NSArray<FileResult*>*) sortFileResults:(NSArray<FileResult*>*)fileResults;

@end

#endif /* Finder_h */

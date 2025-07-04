#ifndef Finder_h
#define Finder_h

#import <Foundation/Foundation.h>
#import "FileTypes.h"
#import "FileResult.h"
#import "FileResultFormatter.h"
#import "FindSettings.h"

#define INVALID_RANGE_MINDEPTH_MAXDEPTH "Invalid range for mindepth and maxdepth"
#define INVALID_RANGE_MINLASTMOD_MAXLASTMOD "Invalid range for minlastmod and maxlastmod"
#define INVALID_RANGE_MINSIZE_MAXSIZE "Invalid range for minsize and maxsize"
#define STARTPATH_NOT_DEFINED "Startpath not defined"
#define STARTPATH_NOT_FOUND "Startpath not found"
#define STARTPATH_NOT_READABLE "Startpath not readable"
#define STARTPATH_NOT_MATCH_FIND_SETTINGS "Startpath does not match find settings"

@interface Finder : NSObject

@property FileTypes *fileTypes;
@property FindSettings *settings;
@property NSStringEncoding textFileEncoding;

- (instancetype) initWithSettings:(FindSettings*)settings error:(NSError**)error;
- (NSArray<FileResult*>*) find:(NSError**)error;
- (void) printMatchingDirs:(NSArray<FileResult*>*)fileResults formatter:(FileResultFormatter*)formatter;
- (void) printMatchingFiles:(NSArray<FileResult*>*)fileResults formatter:(FileResultFormatter*)formatter;

// private methods
- (FileResult*) filterToFileResult:(NSString*)filePath error:(NSError**)error;
- (BOOL) isMatchingArchiveFile:(NSString*)filePath;
- (BOOL) isMatchingDir:(NSString*)dirPath;
- (BOOL) isMatchingFile:(NSString*)filePath;
- (NSArray<NSString*>*) getMatchingDirs:(NSArray<FileResult*>*)fileResults;

@end

#endif /* Finder_h */

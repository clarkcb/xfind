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
- (BOOL) isMatchingDirPathByHidden:(NSString*)dirPath;
- (BOOL) isMatchingDirPathByInPatterns:(NSString*)dirPath;
- (BOOL) isMatchingDirPathByOutPatterns:(NSString*)dirPath;
- (BOOL) isTraversableDirPath:(NSString*)dirPath;
- (BOOL) isMatchingDirPath:(NSString*)dirPath;
- (BOOL) isNullOrMatchingDirPath:(NSString*)dirPath;
- (BOOL) isMatchingFileNameByHidden:(NSString*)fileName;

- (BOOL) isMatchingArchiveExtension:(NSString*)ext;
- (BOOL) isMatchingArchiveExtensionForFilePath:(NSString*)filePath;
- (BOOL) isMatchingArchiveFileName:(NSString*)fileName;
- (BOOL) isMatchingArchiveFileNameForFilePath:(NSString*)fileName;
- (BOOL) isMatchingArchiveFilePath:(NSString*)filePath;
- (BOOL) isMatchingArchiveFileResult:(FileResult*)fileResult;

- (BOOL) isMatchingExtension:(NSString*)ext;
- (BOOL) isMatchingExtensionForFilePath:(NSString*)filePath;
- (BOOL) isMatchingFileName:(NSString*)fileName;
- (BOOL) isMatchingFileNameForFilePath:(NSString*)fileName;
- (BOOL) isMatchingFilePath:(NSString*)filePath;
- (BOOL) isMatchingFileResult:(FileResult*)fileResult;

- (FileResult*) filterArchiveFilePathToFileResult:(NSString*)filePath error:(NSError**)error;
- (FileResult*) filterRegularFilePathToFileResult:(NSString*)filePath fileType:(FileType)fileType error:(NSError**)error;
- (FileResult*) filterToFileResult:(NSString*)filePath error:(NSError**)error;

- (NSArray<NSString*>*) getMatchingDirs:(NSArray<FileResult*>*)fileResults;

@end

#endif /* Finder_h */

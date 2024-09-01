#ifndef FileTypes_h
#define FileTypes_h

#import <Foundation/Foundation.h>
#import <sqlite3.h>
#import "common.h"

@interface FileTypes : NSObject

+ (FileType) fromName:(NSString*)typeName;
+ (NSString*) toName:(FileType)fileType;
- (NSDictionary<NSString*,NSNumber*>*) getFileTypesForQuery:(NSString*)query andParams:(NSArray<NSString*>*)params;
- (FileType) getFileTypeForQuery:(NSString*)query andParams:(NSArray<NSString*>*)params;
- (FileType) getFileTypeForFileName:(NSString*)fileName;
- (FileType) getFileTypeForExtension:(NSString*)fileExt;
- (FileType) getFileType:(NSString*)fileName;
- (BOOL) isArchiveFile:(NSString*)fileName;
- (BOOL) isAudioFile:(NSString*)fileName;
- (BOOL) isBinaryFile:(NSString*)fileName;
- (BOOL) isCodeFile:(NSString*)fileName;
- (BOOL) isFontFile:(NSString*)fileName;
- (BOOL) isImageFile:(NSString*)fileName;
- (BOOL) isTextFile:(NSString*)fileName;
- (BOOL) isVideoFile:(NSString*)fileName;
- (BOOL) isXmlFile:(NSString*)fileName;
- (void) loadNameTypeCache;

@end

#endif /* FileTypes_h */

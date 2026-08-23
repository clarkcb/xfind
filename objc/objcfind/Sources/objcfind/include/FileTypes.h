#ifndef FileTypes_h
#define FileTypes_h

#import <Foundation/Foundation.h>
#import "common.h"
#import "FindConfig.h"

@interface FileTypes : NSObject

- (instancetype) initWithConfig:(FindConfig*)config error:(NSError**)error;
- (NSArray*) loadFileTypesFromJsonFile:(NSString*)fileTypesPath error:(NSError**)error;
+ (FileType) fromName:(NSString*)typeName;
+ (NSString*) toName:(FileType)fileType;
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

@end

#endif /* FileTypes_h */

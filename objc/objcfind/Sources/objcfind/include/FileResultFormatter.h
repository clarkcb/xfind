#ifndef FileResultFormatter_h
#define FileResultFormatter_h

#import <Foundation/Foundation.h>
#import "color.h"
#import "FileResult.h"
#import "FindSettings.h"

@interface FileResultFormatter : NSObject

@property FindSettings *settings;
@property (nonatomic, copy) NSString* ( ^ formatDirPath ) ( NSString * );
@property (nonatomic, copy) NSString* ( ^ formatFileName ) ( NSString * );

- (instancetype) initWithSettings:(FindSettings*)settings;

- (NSString *) formatDirPathWithColor:(NSString*)dirPath;

- (NSString *) formatFileNameWithColor:(NSString*)fileName;

- (NSString *) formatFilePath:(NSString*)filePath;

- (NSString *) formatFileResult:(FileResult*)result;

@end

#endif /* FileResultFormatter_h */

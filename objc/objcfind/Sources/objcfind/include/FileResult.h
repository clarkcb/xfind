#ifndef FileResult_h
#define FileResult_h

#import <Foundation/Foundation.h>
#import "common.h"

@interface FileResult : NSObject

@property NSArray<NSString*> *containers;
@property NSString *filePath;
@property FileType fileType;
@property unsigned long long fileSize;
@property NSDate *lastMod;

- (instancetype) initWithFilePath:(NSString*)filePath fileType:(FileType)fileType fileSize:(unsigned long long)fileSize lastMod:(NSDate*)lastMod;
- (NSString *) description;
- (NSComparisonResult)compareByPath:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareByName:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareBySize:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareByType:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareByLastMod:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;

@end

#endif /* FileResult_h */

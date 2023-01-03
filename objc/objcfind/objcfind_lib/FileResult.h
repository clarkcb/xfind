#import <Foundation/Foundation.h>
#import "common.h"

@interface FileResult : NSObject

@property NSArray<NSString*> *containers;
@property NSString *filePath;
@property FileType fileType;

- (instancetype) initWithFilePath:(NSString*)filePath fileType:(FileType)fileType;
- (NSString *) description;
- (NSComparisonResult)compareByPath:(FileResult *)otherFileResult;
- (NSComparisonResult)compareByName:(FileResult *)otherFileResult;
- (NSComparisonResult)compareByType:(FileResult *)otherFileResult;

@end

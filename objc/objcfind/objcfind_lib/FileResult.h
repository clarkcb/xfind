#import <Foundation/Foundation.h>
#import "common.h"

@interface FileResult : NSObject

@property NSArray<NSString*> *containers;
@property NSString *filePath;
@property FileType fileType;
@property NSDictionary<NSFileAttributeKey, id> *stat;

- (instancetype) initWithFilePath:(NSString*)filePath fileType:(FileType)fileType stat:(NSDictionary<NSFileAttributeKey, id>*)stat;
- (NSString *) description;
- (NSComparisonResult)compareByPath:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareByName:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareBySize:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareByType:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareByLastMod:(FileResult *)otherFileResult caseInsensitive:(BOOL)caseInsensitive;

@end

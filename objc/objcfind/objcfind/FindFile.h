#import <Foundation/Foundation.h>
#import "common.h"

@interface FindFile : NSObject

@property NSArray<NSString*> *containers;
@property NSString *filePath;
@property FileType fileType;

- (instancetype) initWithFilePath:(NSString*)filePath fileType:(FileType)fileType;
- (NSString *) description;

@end

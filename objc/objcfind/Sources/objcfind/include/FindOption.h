#ifndef FindOption_h
#define FindOption_h

#import <Foundation/Foundation.h>

#import "Option.h"

@interface FindOption : NSObject <Option>

@property NSString *shortArg;
@property NSString *longArg;
@property NSString *desc;
@property ArgTokenType argType;

- (instancetype) initWithShortArg:(NSString*)sArg withLongArg:(NSString*)lArg withDesc:(NSString*)desc withArgType:(ArgTokenType)argType;

- (NSString *) sortArg;

@end

#endif /* FindOption_h */

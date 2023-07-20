#ifndef FindOption_h
#define FindOption_h

#import <Foundation/Foundation.h>

@interface FindOption : NSObject

@property NSString *shortArg;
@property NSString *longArg;
@property NSString *desc;

- (instancetype) initWithShortArg:(NSString*)sArg withLongArg:(NSString*)lArg withDesc:(NSString*)desc;

- (NSString *) sortArg;

@end

#endif /* FindOption_h */

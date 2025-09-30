#ifndef ArgToken_h
#define ArgToken_h

#import <Foundation/Foundation.h>
#import "common.h"

@interface ArgToken : NSObject

@property NSString *name;
@property ArgTokenType type;
@property NSObject *value;

- (instancetype) initWithName:(NSString*)name type:(ArgTokenType)type value:(NSObject*)value;

@end

#endif /* ArgToken_h */

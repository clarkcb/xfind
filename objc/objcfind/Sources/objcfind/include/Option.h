#ifndef Option_h
#define Option_h

#import <Foundation/Foundation.h>

#import "common.h"

@protocol Option

@property NSString* shortArg;
@property NSString* longArg;
@property NSString* desc;
@property ArgTokenType argType;

@end

#endif /* Option_h */

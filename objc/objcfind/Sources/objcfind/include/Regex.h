#ifndef Regex_h
#define Regex_h

#import <Foundation/Foundation.h>

@interface Regex : NSObject

@property NSRegularExpression *expression;
@property NSString *pattern;

- (instancetype) initWithPattern:(NSString*) pattern;
- (NSArray<NSTextCheckingResult*>*) matches:(NSString*) s;
- (NSTextCheckingResult *) firstMatch:(NSString*) s;
- (BOOL) test:(NSString*) s;

@end

#endif /* Regex_h */

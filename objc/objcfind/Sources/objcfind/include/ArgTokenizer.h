#ifndef ArgTokenizer_h
#define ArgTokenizer_h

#import <Foundation/Foundation.h>
#import "ArgToken.h"

@interface ArgTokenizer : NSObject
- (instancetype) initWithBoolDict:(NSDictionary<NSString*,NSString*>*)boolDict stringDict:(NSDictionary<NSString*,NSString*>*)stringDict intDict:(NSDictionary<NSString*,NSString*>*)intDict;
- (NSArray<ArgToken*> *) tokenizeArgs:(NSArray<NSString*> *)args error:(NSError **)error;
- (NSArray<ArgToken*> *) tokenizeDictionary:(NSDictionary *)dictionary error:(NSError **)error;
- (NSArray<ArgToken*> *) tokenizeData:(NSData *)data error:(NSError **)error;
- (NSArray<ArgToken*> *) tokenizeFile:(NSString *)filePath error:(NSError **)error;
@end

#endif /* ArgTokenizer_h */

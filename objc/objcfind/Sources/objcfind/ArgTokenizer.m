#import "common.h"
#import "ArgTokenizer.h"
#import "FileUtil.h"

@interface ArgTokenizer ()
// private properties
@property NSDictionary<NSString*,NSString*> *boolDict;
@property NSDictionary<NSString*,NSString*> *stringDict;
@property NSDictionary<NSString*,NSString*> *intDict;

@end

@implementation ArgTokenizer

- (instancetype) initWithBoolDict:(NSDictionary<NSString*,NSString*>*)boolDict stringDict:(NSDictionary<NSString*,NSString*>*)stringDict intDict:(NSDictionary<NSString*,NSString*>*)intDict {
    self = [super init];
    if (self) {
        self.boolDict = boolDict;
        self.stringDict = stringDict;
        self.intDict = intDict;
    }
    return self;
}

- (NSArray<ArgToken*> *) tokenizeArgs:(NSArray<NSString*> *)args error:(NSError **)error {
    NSMutableArray *argTokens = [[NSMutableArray alloc] init];

    int i = 1;
    while (i < [args count]) {
        if (*error) {
            return nil;
        }
        NSString *arg = args[i];
        if ([arg hasPrefix:@"-"]) {
            NSMutableArray *argNames = [NSMutableArray array];
            NSString *argVal = nil;
            if ([arg hasPrefix:@"--"]) {
                if ([arg length] > 2) {
                    // Process long arg
                    arg = [arg substringFromIndex:2];
                    NSArray *parts = [arg componentsSeparatedByString:@"="];
                    if ([parts count] > 0) {
                        arg = [parts objectAtIndex:0];
                    }
                    if ([parts count] > 1) {
                        argVal = [parts objectAtIndex:1];
                    }
                    [argNames addObject:arg];
                } else {
                    setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                    return nil;
                }
            } else if ([arg length] > 1) {
                // Process short arg(s)
                arg = [arg substringFromIndex:1];
                for (NSUInteger i = 0; i < [arg length]; i++) {
                    unichar c = [arg characterAtIndex:i];
                    NSString *cs = [NSString stringWithFormat:@"%C", c];
                    if (self.boolDict[cs]) {
                        [argNames addObject:self.boolDict[cs]];
                    } else if (self.stringDict[cs]) {
                        [argNames addObject:self.stringDict[cs]];
                    } else if (self.intDict[cs]) {
                        [argNames addObject:self.intDict[cs]];
                    } else {
                        setError(error, [NSString stringWithFormat:@"Invalid option: %@", cs]);
                        return nil;
                    }
                }
            } else {
                setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                return nil;
            }

            for (NSString *argName in argNames) {
                if (self.boolDict[argName]) {
                    [argTokens addObject:[[ArgToken alloc] initWithName:argName type:ArgTokenTypeBool value:@(YES)]];
                } else {
                    if (argVal == nil) {
                        if ([args count] > i+1) {
                            argVal = args[i+1];
                            i++;
                        } else {
                            setError(error, [NSString stringWithFormat:@"Missing argument for option %@", arg]);
                            return nil;
                        }
                    }
                    if (self.stringDict[argName] || [argName isEqualToString:@"settings-file"]) {
                        [argTokens addObject:[[ArgToken alloc] initWithName:argName type:ArgTokenTypeStr value:argVal]];
                    } else if (self.intDict[argName]) {
                        [argTokens addObject:[[ArgToken alloc] initWithName:argName type:ArgTokenTypeInt value:@([argVal integerValue])]];
                    } else {
                        setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                        return nil;
                    }
                }
            }

        } else {
            [argTokens addObject:[[ArgToken alloc] initWithName:@"path" type:ArgTokenTypeStr value:arg]];
        }
        i++;
    }
    
    return [NSArray arrayWithArray:argTokens];
}

- (NSArray<ArgToken*> *) tokenizeDictionary:(NSDictionary *)dictionary error:(NSError **)error {
    NSMutableArray *argTokens = [[NSMutableArray alloc] init];
    // keys are sorted so that output is consistent across all versions
    NSArray<NSString*> *keys = [[dictionary allKeys] sortedArrayUsingSelector:@selector(compare:)];
    for (NSString *key in keys) {
        NSObject *val = dictionary[key];
        if (self.boolDict[key]) {
            if ([val isKindOfClass:[NSNumber class]]) {
                NSNumber *num = (NSNumber *)val;
                BOOL b = [num boolValue];
                [argTokens addObject:[[ArgToken alloc] initWithName:key type:ArgTokenTypeBool value:@(b)]];
            } else {
                setError(error, [@"Invalid value for option: " stringByAppendingString:key]);
                return nil;
            }
        } else if (self.stringDict[key] || [key isEqualToString:@"settings-file"]) {
            if ([val isKindOfClass:[NSString class]]) {
                NSString *s = (NSString *)val;
                [argTokens addObject:[[ArgToken alloc] initWithName:key type:ArgTokenTypeStr value:s]];
            } else if ([val isKindOfClass:[NSArray class]]) {
                NSArray *arr = (NSArray *)val;
                for (NSObject *o in arr) {
                    if ([o isKindOfClass:[NSString class]]) {
                        NSString *s = (NSString *)o;
                        [argTokens addObject:[[ArgToken alloc] initWithName:key type:ArgTokenTypeStr value:s]];
                    } else {
                        setError(error, [@"Invalid value for option: " stringByAppendingString:key]);
                        return nil;
                    }
                }
            } else {
                setError(error, [@"Invalid value for option: " stringByAppendingString:key]);
                return nil;
            }
        } else if (self.intDict[key]) {
            if ([val isKindOfClass:[NSNumber class]]) {
                NSNumber *num = (NSNumber *)val;
                NSInteger i = [num integerValue];
                [argTokens addObject:[[ArgToken alloc] initWithName:key type:ArgTokenTypeInt value:@(i)]];
            } else {
                setError(error, [@"Invalid value for option: " stringByAppendingString:key]);
                return nil;
            }
        } else {
            setError(error, [NSString stringWithFormat:@"Invalid option: %@", key]);
            return nil;
        }
    }
    return [NSArray arrayWithArray:argTokens];
}

- (NSArray<ArgToken*> *) tokenizeData:(NSData *)data error:(NSError **)error {
    if (NSClassFromString(@"NSJSONSerialization")) {
        id jsonObject = [NSJSONSerialization
                         JSONObjectWithData:data
                         options:0
                         error:error];
        if (*error) {
            setError(error, @"Unable to parse json");
        }
        
        if (![jsonObject isKindOfClass:[NSDictionary class]]) {
            setError(error, @"Invalid json");
        }
        
        return [self tokenizeDictionary:jsonObject error:error];
    } else {
        setError(error, @"Unable to parse json");
    }
    return nil;
}

- (NSArray<ArgToken*> *) tokenizeFile:(NSString *)filePath error:(NSError **)error {
    NSString *expandedPath = [FileUtil expandPath:filePath];
    if (![[NSFileManager defaultManager] fileExistsAtPath:expandedPath]) {
        setError(error, [@"Settings file not found: " stringByAppendingString:filePath]);
        return nil;
    }
    if (![expandedPath hasSuffix:@".json"]) {
        setError(error, [@"Invalid settings file (must be JSON): " stringByAppendingString:filePath]);
        return nil;
    }
    NSData *data = [NSData dataWithContentsOfFile:expandedPath];
    return [self tokenizeData:data error:error];
}

@end

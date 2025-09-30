#import "ArgTokenizer.h"
#import "FileUtil.h"
#import "FindConfig.h"
#import "Regex.h"
#import "FindOptions.h"

@interface FindOptions ()
// private properties
@property NSArray<FindOption*> *findOptions;
@property NSDictionary<NSString*,NSString*> *longArgDict;
@property NSDictionary *boolActionDict;
@property NSDictionary *stringActionDict;
@property NSDictionary *integerActionDict;
@property ArgTokenizer *argTokenizer;

@end

@implementation FindOptions

- (instancetype) init {
    self = [super init];
    if (self) {
        self.findOptions = [self findOptionsFromJson];
        self.longArgDict = [self getLongArgDict];
        self.boolActionDict = [self getBoolActionDict];
        self.stringActionDict = [self getStringActionDict];
        self.integerActionDict = [self getIntegerActionDict];
        self.argTokenizer = [self getArgTokenizer];
    }
    return self;
}

- (NSArray<FindOption*>*) findOptionsFromJson {
    NSMutableString *findOptionsJsonPath = [NSMutableString stringWithString:getXfindSharedPath()];
    [findOptionsJsonPath appendString:@"/findoptions.json"];
    
    if (![[NSFileManager defaultManager] fileExistsAtPath:findOptionsJsonPath]) {
        return nil;
    }
    
    NSMutableArray *findOptions = [[NSMutableArray alloc] initWithCapacity:44];

    NSData *data = [NSData dataWithContentsOfFile:findOptionsJsonPath];
    
    if (NSClassFromString(@"NSJSONSerialization")) {
        NSError *error = nil;
        id jsonObject = [NSJSONSerialization
                         JSONObjectWithData:data
                         options:0
                         error:&error];
        
        if (error) { /* JSON was malformed, act appropriately here */ }
        
        if ([jsonObject isKindOfClass:[NSDictionary class]]) {
            NSArray *findOptionObjects = jsonObject[@"findoptions"];
            for (NSDictionary *findOptionDict in findOptionObjects) {
                NSString *lArg = findOptionDict[@"long"];
                NSString *sArg = findOptionDict[@"short"];
                NSString *desc = findOptionDict[@"desc"];
                FindOption *so = [[FindOption alloc] initWithShortArg:sArg withLongArg:lArg withDesc:desc];
                [findOptions addObject:(FindOption*)so];
            }
        }
    }
    NSArray *sortedOptions = [findOptions sortedArrayUsingComparator:^NSComparisonResult(FindOption *so1, FindOption *so2) {
        return [[so1 sortArg] compare:[so2 sortArg]];
    }];

    return sortedOptions;
}

- (NSDictionary<NSString*,NSString*>*) getLongArgDict {
    NSMutableDictionary *longArgDict = [[NSMutableDictionary alloc] initWithCapacity:68];
    for (FindOption *so in self.findOptions) {
        longArgDict[so.longArg] = so.longArg;
        if (so.shortArg) {
            longArgDict[so.shortArg] = so.longArg;
        }
    }
    // Add path here because it isn't included in findoptions.json
    longArgDict[@"path"] = @"path";
    return [NSDictionary dictionaryWithDictionary:longArgDict];
}

typedef void (^BoolActionBlockType)(BOOL, FindSettings*);

- (NSDictionary<NSString*,BoolActionBlockType>*) getBoolActionDict {
    return @{
        @"archivesonly" : [^void (BOOL b, FindSettings *ss) {
            ss.archivesOnly = b;
            if (b) ss.includeArchives = true;
        } copy],
        @"colorize" : [^void (BOOL b, FindSettings *ss) { ss.colorize = b; } copy],
        @"debug" : [^void (BOOL b, FindSettings *ss) {
            ss.debug = b;
            if (b) ss.verbose = true;
        } copy],
        @"excludearchives" : [^void (BOOL b, FindSettings *ss) { ss.includeArchives = !b; } copy],
        @"excludehidden" : [^void (BOOL b, FindSettings *ss) { ss.includeHidden = !b; } copy],
        @"followsymlinks" : [^void (BOOL b, FindSettings *ss) { ss.followSymlinks = b; } copy],
        @"help" : [^void (BOOL b, FindSettings *ss) { ss.printUsage = b; } copy],
        @"includearchives" : [^void (BOOL b, FindSettings *ss) { ss.includeArchives = b; } copy],
        @"includehidden" : [^void (BOOL b, FindSettings *ss) { ss.includeHidden = b; } copy],
        @"nocolorize" : [^void (BOOL b, FindSettings *ss) { ss.colorize = !b; } copy],
        @"nofollowsymlinks" : [^void (BOOL b, FindSettings *ss) { ss.followSymlinks = !b; } copy],
        @"noprintdirs" : [^void (BOOL b, FindSettings *ss) { ss.printDirs = !b; } copy],
        @"noprintfiles" : [^void (BOOL b, FindSettings *ss) { ss.printFiles = !b; } copy],
        @"norecursive" : [^void (BOOL b, FindSettings *ss) { ss.recursive = !b; } copy],
        @"printdirs" : [^void (BOOL b, FindSettings *ss) { ss.printDirs = b; } copy],
        @"printfiles" : [^void (BOOL b, FindSettings *ss) { ss.printFiles = b; } copy],
        @"recursive" : [^void (BOOL b, FindSettings *ss) { ss.recursive = b; } copy],
        @"sort-ascending" : [^void (BOOL b, FindSettings *ss) { ss.sortDescending = !b; } copy],
        @"sort-caseinsensitive" : [^void (BOOL b, FindSettings *ss) { ss.sortCaseInsensitive = b; } copy],
        @"sort-casesensitive" : [^void (BOOL b, FindSettings *ss) { ss.sortCaseInsensitive = !b; } copy],
        @"sort-descending" : [^void (BOOL b, FindSettings *ss) { ss.sortDescending = b; } copy],
        @"verbose" : [^void (BOOL b, FindSettings *ss) { ss.verbose = b; } copy],
        @"version" : [^void (BOOL b, FindSettings *ss) { ss.printVersion = b; } copy]
    };
}

typedef void (^StringActionBlockType)(NSString*, FindSettings*);

- (NSDictionary<NSString*,StringActionBlockType>*) getStringActionDict {
    return @{
        @"in-archiveext" : ^void (NSString* s, FindSettings *ss) {
            [ss addInArchiveExtension:s];
        },
        @"in-archivefilepattern" : ^void (NSString* s, FindSettings *ss) {
            [ss.inArchiveFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"in-dirpattern" : ^void (NSString* s, FindSettings *ss) {
            [ss.inDirPatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"in-ext" : ^void (NSString* s, FindSettings *ss) {
            [ss addInExtension:s];
        },
        @"in-filepattern" : ^void (NSString* s, FindSettings *ss) {
            [ss.inFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"in-filetype" : ^void (NSString* s, FindSettings *ss) {
            [ss addInFileType:s];
        },
        @"maxlastmod" : ^void (NSString* s, FindSettings *ss) {
            [ss setMaxLastModFromString:s];
        },
        @"minlastmod" : ^void (NSString* s, FindSettings *ss) {
            [ss setMinLastModFromString:s];
        },
        @"out-archiveext" : ^void (NSString* s, FindSettings *ss) {
            [ss addOutArchiveExtension:s];
        },
        @"out-archivefilepattern" : ^void (NSString* s, FindSettings *ss) {
            [ss.outArchiveFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"out-dirpattern" : ^void (NSString* s, FindSettings *ss) {
            [ss.outDirPatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"out-ext" : ^void (NSString* s, FindSettings *ss) {
            [ss addOutExtension:s];
        },
        @"out-filepattern" : ^void (NSString* s, FindSettings *ss) {
            [ss.outFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"out-filetype" : ^void (NSString* s, FindSettings *ss) {
            [ss addOutFileType:s];
        },
        @"path" : ^void (NSString* s, FindSettings *ss) {
            [ss addPath:s];
        },
        @"sort-by" : ^void (NSString* s, FindSettings *ss) {
            [ss setSortByFromName:s];
        }
    };
}

typedef void (^IntegerActionBlockType)(NSInteger, FindSettings*);

- (NSDictionary<NSString*,IntegerActionBlockType>*) getIntegerActionDict {
    return @{
        @"maxdepth" : [^void (NSInteger i, FindSettings *ss) { ss.maxDepth = i; } copy],
        @"maxsize" : [^void (NSInteger i, FindSettings *ss) { ss.maxSize = i; } copy],
        @"mindepth" : [^void (NSInteger i, FindSettings *ss) { ss.minDepth = i; } copy],
        @"minsize" : [^void (NSInteger i, FindSettings *ss) { ss.minSize = i; } copy]
    };
}

- (ArgTokenizer *) getArgTokenizer {
    NSMutableDictionary *boolDict = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *stringDict = [[NSMutableDictionary alloc] init];
    stringDict[@"path"] = @"path";
    NSMutableDictionary *intDict = [[NSMutableDictionary alloc] init];
    
    for (FindOption *fo in self.findOptions) {
        if (self.boolActionDict[fo.longArg]) {
            boolDict[fo.longArg] = fo.longArg;
            if (fo.shortArg) {
                boolDict[fo.shortArg] = fo.longArg;
            }
        } else if (self.stringActionDict[fo.longArg]) {
            stringDict[fo.longArg] = fo.longArg;
            if (fo.shortArg) {
                stringDict[fo.shortArg] = fo.longArg;
            }
        } else if (self.integerActionDict[fo.longArg]) {
            intDict[fo.longArg] = fo.longArg;
            if (fo.shortArg) {
                intDict[fo.shortArg] = fo.longArg;
            }
        }
    }

    return [[ArgTokenizer alloc] initWithBoolDict:boolDict stringDict:stringDict intDict:intDict];
}

- (void) applyArgTokenToSettings:(ArgToken *)argToken settings:(FindSettings *)settings error:(NSError **)error {
    if (argToken.type == ArgTokenTypeBool) {
        if ([argToken.value isKindOfClass:[NSNumber class]]) {
            NSNumber *num = (NSNumber *)argToken.value;
            BOOL b = [num boolValue];
            void(^block)(BOOL, FindSettings*) = self.boolActionDict[argToken.name];
            block(b, settings);
        } else {
            setError(error, [@"Invalid value for option: " stringByAppendingString:argToken.name]);
            return;
        }
    } else if (argToken.type == ArgTokenTypeStr) {
        if ([argToken.value isKindOfClass:[NSString class]]) {
            NSString *s = (NSString *)argToken.value;
            void(^block)(NSString *, FindSettings *) = self.stringActionDict[argToken.name];
            block(s, settings);
        } else if ([argToken.value isKindOfClass:[NSArray class]]) {
            NSArray *arr = (NSArray *)argToken.value;
            for (NSObject *o in arr) {
                if ([o isKindOfClass:[NSString class]]) {
                    NSString *s = (NSString *)argToken.value;
                    void(^block)(NSString *, FindSettings *) = self.stringActionDict[argToken.name];
                    block(s, settings);
                } else {
                    setError(error, [@"Invalid value for option: " stringByAppendingString:argToken.name]);
                    return;
                }
            }
        } else {
            setError(error, [@"Invalid value for option: " stringByAppendingString:argToken.name]);
            return;
        }
    } else if (argToken.type == ArgTokenTypeInt) {
        if ([argToken.value isKindOfClass:[NSNumber class]]) {
            NSNumber *num = (NSNumber *)argToken.value;
            NSInteger i = [num integerValue];
            void(^block)(NSInteger, FindSettings*) = self.integerActionDict[argToken.name];
            block(i, settings);
        } else {
            setError(error, [@"Invalid value for option: " stringByAppendingString:argToken.name]);
            return;
        }
    } else {
        setError(error, [@"Invalid option: " stringByAppendingString:argToken.name]);
        return;
    }
}

- (void) updateSettingsFromArgTokens:(FindSettings *)settings argTokens:(NSArray<ArgToken*> *)argTokens error:(NSError **)error {
    for (ArgToken *argToken in argTokens) {
        if ([argToken.name isEqualToString:@"settings-file"]) {
            [self updateSettingsFromFile:settings filePath:[NSString stringWithFormat:@"%@", argToken.value] error:error];
        } else {
            [self applyArgTokenToSettings:argToken settings:settings error:error];
        }
        if (*error) {
            return;
        }
    }
}

- (void) updateSettingsFromDictionary:(FindSettings *)settings dictionary:(NSDictionary *)dictionary error:(NSError **)error {
    NSArray<ArgToken*> *argTokens = [self.argTokenizer tokenizeDictionary:dictionary error:error];
    if (*error) {
        return;
    }
    [self updateSettingsFromArgTokens:settings argTokens:argTokens error:error];
}

- (void) updateSettingsFromData:(FindSettings *)settings data:(NSData *)data error:(NSError **)error {
    NSArray<ArgToken*> *argTokens = [self.argTokenizer tokenizeData:data error:error];
    if (*error) {
        return;
    }
    [self updateSettingsFromArgTokens:settings argTokens:argTokens error:error];
}

- (FindSettings *) settingsFromData:(NSData *)data error:(NSError **)error {
    FindSettings *settings = [[FindSettings alloc] init];
    [self updateSettingsFromData:settings data:data error:error];
    return settings;
}

- (void) updateSettingsFromFile:(FindSettings *)settings filePath:(NSString *)filePath error:(NSError **)error {
    NSArray<ArgToken*> *argTokens = [self.argTokenizer tokenizeFile:filePath error:error];
    if (*error) {
        return;
    }
    [self updateSettingsFromArgTokens:settings argTokens:argTokens error:error];
}

- (FindSettings *) settingsFromFile:(NSString *)filePath error:(NSError **)error {
    FindSettings *settings = [[FindSettings alloc] init];
    [self updateSettingsFromFile:settings filePath:filePath error:error];
    return settings;
}

- (void) updateSettingsFromArgs:(FindSettings *)settings args:(NSArray *)args error:(NSError **)error {
    NSArray<ArgToken*> *argTokens = [self.argTokenizer tokenizeArgs:args error:error];
    if (*error) {
        return;
    }
    [self updateSettingsFromArgTokens:settings argTokens:argTokens error:error];
}

- (FindSettings *) settingsFromArgs:(NSArray<NSString*> *)args error:(NSError **)error {
    FindSettings *settings = [[FindSettings alloc] init];
    // default printFiles to true since running as cli
    settings.printFiles = true;
    [self updateSettingsFromArgs:settings args:args error:error];
    return settings;
}

- (NSString*) getUsageString {
    NSMutableString *s = [[NSMutableString alloc] initWithString:@"\nUsage:\n"];
    [s appendString:@" objcfind [options] <path> [<path> ...]\n\n"];
    [s appendString:@"Options:\n"];
    NSMutableArray *optStrings = [NSMutableArray array];
    long longest = 0;
    for (FindOption *so in self.findOptions) {
        NSMutableString *optString = [[NSMutableString alloc] init];
        if (so.shortArg) {
            [optString appendFormat:@"-%@,", so.shortArg];
        }
        [optString appendFormat:@"--%@", so.longArg];
        if ([optString length] > longest) {
            longest = [optString length];
        }
        [optStrings addObject:optString];
    }
    // For some reason, length-specified fields don't work in NSString format strings,
    // so forced to use char * and sprintf
    char *metaString = " %%-%lus  %%s\n";
    char templateString[20];
    sprintf(templateString, metaString, longest);
    for (int i=0; i < [self.findOptions count]; i++) {
        NSString *optString = optStrings[i];
        NSString *optDesc = self.findOptions[i].desc;
        long formatLen = [optString length] + [optDesc length] + 5;
        char formatString[formatLen];
        sprintf(formatString, templateString, [optString UTF8String], [optDesc UTF8String]);
        [s appendString:[NSString stringWithUTF8String:formatString]];
    }
    return [NSString stringWithString:s];
}

- (void) usage:(int)code {
    logMsg([self getUsageString]);
    exit(code);
}

@end

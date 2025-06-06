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

- (void) applySetting:(NSString *)name obj:(NSObject *)obj settings:(FindSettings *)settings error:(NSError **)error {
    if (self.longArgDict[name]) {
        if (self.boolActionDict[name]) {
            if ([obj isKindOfClass:[NSNumber class]]) {
                NSNumber *num = (NSNumber *)obj;
                BOOL b = [num boolValue];
                void(^block)(BOOL, FindSettings*) = self.boolActionDict[name];
                block(b, settings);
            } else {
                setError(error, [@"Invalid value for option: " stringByAppendingString:name]);
                return;
            }
        } else if (self.stringActionDict[name]) {
            if ([obj isKindOfClass:[NSString class]]) {
                NSString *s = (NSString *)obj;
                void(^block)(NSString *, FindSettings *) = self.stringActionDict[name];
                block(s, settings);
            } else if ([obj isKindOfClass:[NSArray class]]) {
                NSArray *arr = (NSArray *)obj;
                for (NSObject *o in arr) {
                    [self applySetting:name obj:o settings:settings error:error];
                    if (*error) {
                        return;
                    }
                }
            } else {
                setError(error, [@"Invalid value for option: " stringByAppendingString:name]);
                return;
            }
        } else if (self.integerActionDict[name]) {
            if ([obj isKindOfClass:[NSNumber class]]) {
                NSNumber *num = (NSNumber *)obj;
                NSInteger i = [num integerValue];
                void(^block)(NSInteger, FindSettings*) = self.integerActionDict[name];
                block(i, settings);
            } else {
                setError(error, [@"Invalid value for option: " stringByAppendingString:name]);
                return;
            }
        } else {
            setError(error, [@"Invalid option: " stringByAppendingString:name]);
            return;
        }
    } else {
        setError(error, [@"Invalid option: " stringByAppendingString:name]);
    }
}

- (void) updateSettingsFromData:(NSData *)data settings:(FindSettings *)settings error:(NSError **)error {
    if (NSClassFromString(@"NSJSONSerialization")) {
        id jsonObject = [NSJSONSerialization
                         JSONObjectWithData:data
                         options:0
                         error:error];
        
        if (*error) {
            setError(error, @"Unable to parse json");
            return;
        }
        
        if (![jsonObject isKindOfClass:[NSDictionary class]]) {
            setError(error, @"Invalid json");
            return;
        }
        
        // keys are sorted so that output is consistent across all versions
        NSArray<NSString*> *keys = [[jsonObject allKeys] sortedArrayUsingSelector:@selector(compare:)];
        // First check for invalid keys
        for (NSString *key in keys) {
            if (!self.longArgDict[key]) {
                setError(error, [@"Invalid option: " stringByAppendingString:key]);
                return;
            }
        }
        for (NSString *key in keys) {
            NSObject *val = jsonObject[key];
            [self applySetting:key obj:val settings:settings error:error];
            if (*error) {
                return;
            }
        }
    }
}

- (FindSettings *) settingsFromData:(NSData *)data error:(NSError **)error {
    FindSettings *settings = [[FindSettings alloc] init];
    [self updateSettingsFromData:data settings:settings error:error];
    return settings;
}

- (void) updateSettingsFromFile:(NSString *)settingsFilePath settings:(FindSettings *)settings error:(NSError **)error {
    NSString *expandedPath = [FileUtil expandPath:settingsFilePath];
    if (![[NSFileManager defaultManager] fileExistsAtPath:expandedPath]) {
        setError(error, [@"Settings file not found: " stringByAppendingString:settingsFilePath]);
        return;
    }
    if (![expandedPath hasSuffix:@".json"]) {
        setError(error, [@"Invalid settings file (must be JSON): " stringByAppendingString:settingsFilePath]);
        return;
    }
    NSData *data = [NSData dataWithContentsOfFile:expandedPath];
    [self updateSettingsFromData:data settings:settings error:error];
    if (*error) {
        if ([[*error domain] isEqualToString:@"Unable to parse JSON"] || [[*error domain] isEqualToString:@"Invalid JSON"]) {
            NSString *newErr = [[[*error domain] stringByAppendingString:@" in settings file: "] stringByAppendingString:settingsFilePath];
            setError(error, newErr);
        }
    }
}

- (FindSettings *) settingsFromFile:(NSString *)settingsFilePath error:(NSError **)error {
    FindSettings *settings = [[FindSettings alloc] init];
    [self updateSettingsFromFile:settingsFilePath settings:settings error:error];
    return settings;
}

- (FindSettings *) settingsFromArgs:(NSArray<NSString*> *)args error:(NSError **)error {
    FindSettings *settings = [[FindSettings alloc] init];
    // default printFiles to true since running as cli
    settings.printFiles = true;

    int i = 1;
    while (i < [args count]) {
        if (*error) {
            return nil;
        }
        NSString *arg = args[i];
        if ([arg hasPrefix:@"-"]) {
            while ([arg hasPrefix:@"-"] && [arg length] > 1) {
                arg = [arg substringFromIndex:1];
            }
            if (self.longArgDict[arg]) {
                NSString *longArg = self.longArgDict[arg];
                if (self.boolActionDict[longArg]) {
                    void(^block)(BOOL, FindSettings *) = self.boolActionDict[longArg];
                    block(true, settings);
                } else {
                    NSString *argVal = @"";
                    if ([args count] > i+1) {
                        argVal = args[i+1];
                        i++;
                    } else {
                        setError(error, [NSString stringWithFormat:@"Missing argument for option %@", arg]);
                        return nil;
                    }
                    if (self.stringActionDict[longArg]) {
                        void(^block)(NSString *, FindSettings *) = self.stringActionDict[longArg];
                        block(argVal, settings);
                    } else if (self.integerActionDict[longArg]) {
                        void(^block)(NSInteger, FindSettings *) = self.integerActionDict[longArg];
                        block([argVal integerValue], settings);
                    } else if ([longArg isEqualToString:@"settings-file"]) {
                        [self updateSettingsFromFile:argVal settings:settings error:error];
                        if (*error) {
                            return nil;
                        }
                    } else {
                        setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                        return nil;
                    }
                }
            } else {
                setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                return nil;
            }
        } else {
            [settings addPath:args[i]];
        }
        i++;
    }
    
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

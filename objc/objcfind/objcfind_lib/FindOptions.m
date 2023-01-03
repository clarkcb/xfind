#import "config.h"
#import "Regex.h"
#import "FindOptions.h"

@interface FindOptions ()
// private properties
@property NSArray<FindOption*> *findOptions;
@property NSDictionary<NSString*,NSString*> *longArgDict;
@property NSDictionary *argActionDict;
@property NSDictionary *boolFlagActionDict;

@end

@implementation FindOptions

- (instancetype) init {
    self = [super init];
    if (self) {
        self.findOptions = [self findOptionsFromJson];
        self.longArgDict = [self getLongArgDict];
        self.argActionDict = [self getArgActionDict];
        self.boolFlagActionDict = [self getBoolFlagActionDict];
    }
    return self;
}

- (NSArray<FindOption*>*) findOptionsFromJson {
    NSMutableString *findOptionsJsonPath = [NSMutableString stringWithUTF8String:SHAREDPATH];
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

- (void) applySetting:(NSString *)name obj:(NSObject *)obj settings:(FindSettings *)settings {
    if ([obj isKindOfClass:[NSString class]]) {
        if ([name isEqualToString:@"path"]) {
            [settings addPath:(NSString*)obj];
        } else if (self.argActionDict[name]) {
            void(^block)(NSObject *, FindSettings *) = self.argActionDict[name];
            block(obj, settings);
        }
    } else if ([obj isKindOfClass:[NSNumber class]]) {
        NSNumber *num = (NSNumber *)obj;
        if (self.argActionDict[name]) {
            void(^block)(NSString* s, FindSettings* ss) = self.argActionDict[name];
            block([num description], settings);
        } else if (self.boolFlagActionDict[name]) {
            BOOL b = [num boolValue];
            void(^block)(BOOL, FindSettings *) = self.boolFlagActionDict[name];
            block(b, settings);
        }
    } else if ([obj isKindOfClass:[NSArray class]]) {
        if (self.argActionDict[name]) {
            void(^block)(NSObject *, FindSettings *) = self.argActionDict[name];
            NSArray *arr = (NSArray *)obj;
            for (NSObject *o in arr) {
                block([o description], settings);
            }
        }
    }
}

- (void) settingsFromFile:(NSString *)settingsFilePath settings:(FindSettings *)settings {
    if (![[NSFileManager defaultManager] fileExistsAtPath:settingsFilePath]) {
        return;
    }
    
    NSData *data = [NSData dataWithContentsOfFile:settingsFilePath];
    
    [self settingsFromData:data settings:settings];
}

- (void) settingsFromData:(NSData *)data settings:(FindSettings *)settings {
    if (NSClassFromString(@"NSJSONSerialization")) {
        NSError *error = nil;
        id jsonObject = [NSJSONSerialization
                         JSONObjectWithData:data
                         options:0
                         error:&error];
        
        if (error) { /* JSON was malformed, act appropriately here */ }
        
        if ([jsonObject isKindOfClass:[NSDictionary class]]) {
            for (NSString *key in jsonObject) {
                NSObject *val = jsonObject[key];
                [self applySetting:key obj:val settings:settings];
            }
        }
    }
}

- (NSDictionary<NSString*,NSString*>*) getLongArgDict {
    NSMutableDictionary *longArgDict = [[NSMutableDictionary alloc] initWithCapacity:68];
    for (FindOption *so in self.findOptions) {
        longArgDict[so.longArg] = so.longArg;
        if (so.shortArg) {
            longArgDict[so.shortArg] = so.longArg;
        }
    }
    return [NSDictionary dictionaryWithDictionary:longArgDict];
}

typedef void (^ArgActionBlockType)(NSString*, FindSettings*);

- (NSDictionary<NSString*,ArgActionBlockType>*) getArgActionDict {
    return [[NSDictionary alloc] initWithObjectsAndKeys:
            ^void (NSString* s, FindSettings *ss) {
                [ss addInArchiveExtension:s];
            }, @"in-archiveext",
            ^void (NSString* s, FindSettings *ss) {
                [ss.inArchiveFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"in-archivefilepattern",
            ^void (NSString* s, FindSettings *ss) {
                [ss.inDirPatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"in-dirpattern",
            ^void (NSString* s, FindSettings *ss) {
                [ss addInExtension:s];
            }, @"in-ext",
            ^void (NSString* s, FindSettings *ss) {
                [ss.inFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"in-filepattern",
            ^void (NSString* s, FindSettings *ss) {
                [ss addInFileType:s];
            }, @"in-filetype",
            ^void (NSString* s, FindSettings *ss) {
                [ss addOutArchiveExtension:s];
            }, @"out-archiveext",
            ^void (NSString* s, FindSettings *ss) {
                [ss.outArchiveFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"out-archivefilepattern",
            ^void (NSString* s, FindSettings *ss) {
                [ss.outDirPatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"out-dirpattern",
            ^void (NSString* s, FindSettings *ss) {
                [ss addOutExtension:s];
            }, @"out-ext",
            ^void (NSString* s, FindSettings *ss) {
                [ss.outFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"out-filepattern",
            ^void (NSString* s, FindSettings *ss) {
                [ss addOutFileType:s];
            }, @"out-filetype",
            ^void (NSString* s, FindSettings *ss) {
                [ss.paths addObject:s];
            }, @"path",
            ^void (NSString* s, FindSettings *ss) {
                [ss setSortByFromName:s];
            }, @"sort-by",
//            ^void (NSString* s, FindSettings *ss) {
//                [self settingsFromFile:s settings:ss];
//            }, @"settings-file",
            nil];
}

typedef void (^BoolFlagActionBlockType)(BOOL, FindSettings*);

- (NSDictionary<NSString*,BoolFlagActionBlockType>*) getBoolFlagActionDict {
    return [[NSDictionary alloc] initWithObjectsAndKeys:
            [^void (BOOL b, FindSettings *ss) {
                ss.archivesOnly = b;
                if (b) ss.includeArchives = true;
            } copy], @"archivesonly",
            [^void (BOOL b, FindSettings *ss) {
                ss.debug = b;
                if (b) ss.verbose = true;
            } copy], @"debug",
            [^void (BOOL b, FindSettings *ss) { ss.includeArchives = !b; } copy], @"excludearchives",
            [^void (BOOL b, FindSettings *ss) { ss.excludeHidden = b; } copy], @"excludehidden",
            [^void (BOOL b, FindSettings *ss) { ss.printUsage = b; } copy], @"help",
            [^void (BOOL b, FindSettings *ss) { ss.includeArchives = b; } copy], @"includearchives",
            [^void (BOOL b, FindSettings *ss) { ss.excludeHidden = !b; } copy], @"includehidden",
            [^void (BOOL b, FindSettings *ss) { ss.listDirs = b; } copy], @"listdirs",
            [^void (BOOL b, FindSettings *ss) { ss.listFiles = b; } copy], @"listfiles",
            [^void (BOOL b, FindSettings *ss) { ss.recursive = !b; } copy], @"norecursive",
            [^void (BOOL b, FindSettings *ss) { ss.recursive = b; } copy], @"recursive",
            [^void (BOOL b, FindSettings *ss) { ss.sortDescending = !b; } copy], @"sort-ascending",
            [^void (BOOL b, FindSettings *ss) { ss.sortDescending = b; } copy], @"sort-descending",
            [^void (BOOL b, FindSettings *ss) { ss.verbose = b; } copy], @"verbose",
            [^void (BOOL b, FindSettings *ss) { ss.printVersion = b; } copy], @"version",
            nil];
}

- (FindSettings *) settingsFromArgs:(NSArray<NSString*> *)args error:(NSError **)error {
    FindSettings *settings = [[FindSettings alloc] init];
    // default listFiles to true since running as cli
    settings.listFiles = true;

    int i = 1;
    while (i < [args count]) {
        NSString *arg = args[i];
        if ([arg hasPrefix:@"-"]) {
            while ([arg hasPrefix:@"-"] && [arg length] > 1) {
                arg = [arg substringFromIndex:1];
            }
            if (self.longArgDict[arg]) {
                //logMsg([NSString stringWithFormat:@"Option in longArgDict: %@", arg]);
                NSString *longArg = self.longArgDict[arg];
                if (self.argActionDict[longArg] || [longArg isEqualToString:@"settings-file"]) {
                    if ([args count] > i+1) {
                        NSString *secondArg = args[i+1];
                        if (self.argActionDict[longArg]) {
                            void(^block)(NSString *, FindSettings *) = self.argActionDict[longArg];
                            block(secondArg, settings);
                        } else {
                            [self settingsFromFile:secondArg settings:settings];
                        }
                        i++;
                    } else {
                        setError(error, [NSString stringWithFormat:@"Missing argument for option %@", arg]);
                        return nil;
                    }
                } else if (self.boolFlagActionDict[longArg]) {
                    void(^block)(BOOL, FindSettings *) = self.boolFlagActionDict[longArg];
                    block(true, settings);
                } else {
                    setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                    return nil;
                }
            } else {
                setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                return nil;
            }
        } else {
            [settings.paths addObject:args[i]];
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

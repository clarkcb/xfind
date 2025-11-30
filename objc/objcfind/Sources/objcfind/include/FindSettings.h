#ifndef FindSettings_h
#define FindSettings_h

#import <Foundation/Foundation.h>
#import "common.h"
#import "FileTypes.h"
#import "Regex.h"

@interface FindSettings : NSObject
//@private
//    BOOL _archivesOnly;
//    BOOL _debug;
//    NSMutableString *_startPath;
//}

@property(nonatomic) BOOL archivesOnly;
@property(nonatomic) BOOL colorize;
@property(nonatomic) BOOL debug;
@property(nonatomic) Color dirColor;
@property(nonatomic) Color extColor;
@property(nonatomic) Color fileColor;
@property(nonatomic) BOOL followSymlinks;
@property(nonatomic) BOOL includeArchives;
@property(nonatomic) BOOL includeHidden;
@property(nonatomic) BOOL printDirs;
@property(nonatomic) BOOL printFiles;
@property(nonatomic) BOOL printUsage;
@property(nonatomic) BOOL printVersion;
@property(nonatomic) BOOL recursive;
@property(nonatomic) BOOL sortCaseInsensitive;
@property(nonatomic) BOOL sortDescending;
@property(nonatomic) BOOL verbose;

@property(nonatomic) NSInteger maxDepth;
@property(nonatomic) NSDate *maxLastMod;
@property(nonatomic) NSInteger maxSize;
@property(nonatomic) NSInteger minDepth;
@property(nonatomic) NSDate *minLastMod;
@property(nonatomic) NSInteger minSize;

@property(nonatomic) NSMutableArray<NSString*> *inArchiveExtensions;
@property(nonatomic) NSMutableArray<Regex*> *inArchiveFilePatterns;
@property(nonatomic) NSMutableArray<Regex*> *inDirPatterns;
@property(nonatomic) NSMutableArray<NSString*> *inExtensions;
@property(nonatomic) NSMutableArray<Regex*> *inFilePatterns;
@property(nonatomic) NSMutableArray<NSNumber*> *inFileTypes;
@property(nonatomic) NSMutableArray<NSString*> *outArchiveExtensions;
@property(nonatomic) NSMutableArray<Regex*> *outArchiveFilePatterns;
@property(nonatomic) NSMutableArray<Regex*> *outDirPatterns;
@property(nonatomic) NSMutableArray<NSString*> *outExtensions;
@property(nonatomic) NSMutableArray<Regex*> *outFilePatterns;
@property(nonatomic) NSMutableArray<NSNumber*> *outFileTypes;
@property(nonatomic) NSMutableArray<NSString*> *paths;
@property(nonatomic) SortBy sortBy;

- (instancetype) init;
-(NSString *) description;

- (void) addExtensions:(NSString*)ext toArr:(NSMutableArray *)arr;
- (void) addInArchiveExtension:(NSString*)ext;
- (void) addInArchiveFilePattern:(NSString*)pattern;
- (void) addInDirPattern:(NSString*)pattern;
- (void) addInExtension:(NSString*)ext;
- (void) addInFilePattern:(NSString*)pattern;
- (void) addOutArchiveExtension:(NSString*)ext;
- (void) addOutArchiveFilePattern:(NSString*)pattern;
- (void) addOutDirPattern:(NSString*)pattern;
- (void) addOutExtension:(NSString*)ext;
- (void) addOutFilePattern:(NSString*)pattern;
- (void) addPath:(NSString*)path;

- (void) setMaxDepthFromString:(NSString*)depthStr;
- (void) setMaxLastModFromString:(NSString*)sizeStr;
- (void) setMaxSizeFromString:(NSString*)sizeStr;
- (void) setMinDepthFromString:(NSString*)depthStr;
- (void) setMinLastModFromString:(NSString*)sizeStr;
- (void) setMinSizeFromString:(NSString*)sizeStr;

+ (SortBy) getSortByFromName:(NSString*)sortByName;
+ (NSString *) getNameFromSortBy:(SortBy)sortBy;
- (void) setSortByFromName:(NSString*)sortByName;

- (void) addFileType:(NSString*)typeName toArr:(NSMutableArray *)arr;
- (void) addInFileType:(NSString*)typeName;
- (void) addOutFileType:(NSString*)typeName;

- (BOOL) needLastMod;
- (BOOL) needSize;

+ (NSString *) lastModToNSString:(NSDate *)lastMod;
+ (NSString *) fileTypesArrayToNSString:(NSArray<NSNumber*>*)arr;
//- (void) setArchivesOnly: (BOOL)b;

@end

#endif /* FindSettings_h */

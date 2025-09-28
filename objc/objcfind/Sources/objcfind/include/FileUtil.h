#ifndef FileUtil_h
#define FileUtil_h

//
//  FileUtil.h
//  objcfind
//
//  Created by Cary Clark on 1/14/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "FindConfig.h"
#import "FindSettings.h"

#define DOT                @"."
#define DOT_DOT            @".."
#define SEPARATOR          @"/"
#define DOT_SEPARATOR      @"./"
#define DOT_DOT_SEPARATOR  @"../"

@interface FileUtil : NSObject

+ (NSSet<NSString*>*) dotDirs;
+ (NSString*) expandPath:(NSString*)filePath;
+ (NSString*) contractPath:(NSString*)filePath;
+ (NSString*) absolutePath:(NSString*)filePath;
+ (NSString*) relativePath:(NSString*)filePath to:(NSString*)toPath;
+ (NSString*) getExtension:(NSString*)fileName;
+ (BOOL) hasExtension:(NSString*)fileName ext:(NSString*)ext;
+ (NSArray<NSString*>*) contentsForPath:(NSString*)filePath error:(NSError**)error;
+ (NSDirectoryEnumerator*) enumeratorForPath:(NSString*)filePath settings:(FindSettings*)settings;
+ (NSDictionary<NSFileAttributeKey, id>*) getFileAttributes:(NSString*)filePath error:(NSError**)error;
+ (BOOL) exists:(NSString*)filePath;
+ (BOOL) allExist:(NSArray<NSString*>*)filePaths;
+ (BOOL) isDirectory:(NSString*)filePath;
+ (BOOL) isDotDir:(NSString*)filePath;
+ (BOOL) isHiddenName:(NSString*)name;
+ (BOOL) isHiddenPath:(NSString*)filePath;
+ (BOOL) isReadableFile:(NSString*)filePath;
+ (BOOL) allReadable:(NSArray<NSString*>*)filePaths;
+ (BOOL) isSymlink:(NSString*)filePath;
+ (NSString*) getSymlinkTarget:(NSString*)symlinkPath;
+ (NSString*) joinPath:(NSString*)path childPath:(NSString*)childPath;
+ (NSString*) normalizePath:(NSString*)path;
+ (NSDirectoryEnumerationOptions) optionsForSettings:(FindSettings*)settings;

@end

#endif /* FileUtil_h */

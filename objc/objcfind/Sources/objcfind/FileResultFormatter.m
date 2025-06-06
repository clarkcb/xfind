//
//  FileResultFormatter.m
//  objcfind
//
//  Created by Cary Clark on 5/10/25.
//

#import <Foundation/Foundation.h>
#import "FileResultFormatter.h"
#import "FileUtil.h"

@implementation FileResultFormatter

- (instancetype) initWithSettings:(FindSettings*)settings {
    self = [super init];
    if (self) {
        // necessary to reference other methods in self from constructor
        __weak typeof(self) weakSelf = self;

        self.settings = settings;
        if (settings.colorize && settings.inDirPatterns.count > 0) {
            self.formatDirPath = ^( NSString* dirPath) { return [weakSelf formatDirPathWithColor:dirPath]; };
        } else {
            self.formatDirPath = ^( NSString* dirPath) { return dirPath; };
        }
        if (settings.colorize && (settings.inExtensions.count > 0 || settings.inFilePatterns.count > 0)) {
            self.formatFileName = ^( NSString* fileName) { return [weakSelf formatFileNameWithColor:fileName]; };
        } else {
            self.formatFileName = ^( NSString* fileName) { return fileName; };
        }
    }
    return self;
}

- (NSString *) colorize:(NSString*)s matchStartIndex:(long)matchStartIndex matchEndIndex:(long)matchEndIndex {
    NSMutableString *c = [NSMutableString string];
    if (matchStartIndex > 0) {
        [c appendString:[s substringToIndex:matchStartIndex]];
    }
    long matchLength = matchEndIndex - matchStartIndex;
    [c appendFormat:@"%s", ANSI_GREEN];
    [c appendString:[s substringWithRange:NSMakeRange(matchStartIndex, matchLength)]];
    [c appendString:ANSI_RESET];

    if (matchEndIndex < [s length]) {
        [c appendString:[s substringFromIndex:matchEndIndex]];
    }

    return [NSString stringWithString:c];
}

- (NSString *) formatDirPathWithColor:(NSString*)dirPath {
    NSString *formattedDirPath = dirPath;
    if ([formattedDirPath isEqual:@""]) {
        formattedDirPath = @".";
    } else {
        for (Regex *p in self.settings.inDirPatterns) {
            NSTextCheckingResult *m = [p firstMatch:formattedDirPath];
            if (m != nil) {
                formattedDirPath = [self colorize:formattedDirPath matchStartIndex:m.range.location matchEndIndex:m.range.location + m.range.length];
                break;
            }
        }
    }
    return formattedDirPath;
}

- (NSString *) formatFileNameWithColor:(NSString*)fileName {
    NSString *formattedFileName = fileName;
    for (Regex *p in self.settings.inFilePatterns) {
        NSTextCheckingResult *m = [p firstMatch:formattedFileName];
        if (m != nil) {
            formattedFileName = [self colorize:formattedFileName matchStartIndex:m.range.location matchEndIndex:m.range.location + m.range.length];
            break;
        }
    }
    if ([self.settings.inExtensions count] > 0) {
        int idx = (int)([formattedFileName rangeOfString:@"." options:NSBackwardsSearch].location);
        if (idx > 0 && idx < formattedFileName.length) {
            formattedFileName = [self colorize:formattedFileName matchStartIndex:(idx + 1) matchEndIndex:formattedFileName.length];
        }
    }
    return formattedFileName;
}

- (NSString *) formatFilePath:(NSString*)filePath {
    NSString *parent = self.formatDirPath([filePath stringByDeletingLastPathComponent]);
    NSString *fileName = self.formatFileName([filePath lastPathComponent]);
    return [FileUtil joinPath:parent childPath:fileName];
}

- (NSString *) formatFileResult:(FileResult*)result {
    return [self formatFilePath:[result filePath]];
}

@end

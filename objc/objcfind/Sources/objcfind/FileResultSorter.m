//
//  FileResultSorter.m
//  objcfind
//
//  Created by Cary Clark on 7/1/25.
//

#import <Foundation/Foundation.h>
#import "FileResultSorter.h"
#import "FileUtil.h"

@implementation FileResultSorter

- (instancetype) initWithSettings:(FindSettings*)settings {
    self = [super init];
    if (self) {
        self.settings = settings;
    }
    return self;
}

- (NSComparisonResult (^)(FileResult*, FileResult*)) getFileResultComparator {
    if (self.settings.sortDescending) {
        if (self.settings.sortBy == SortByFileName) {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr2 compareByName:fr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByFileSize) {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr2 compareBySize:fr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByFileType) {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr2 compareByType:fr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByLastMod) {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr2 compareByLastMod:fr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr2 compareByPath:fr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        }
    } else {
        if (self.settings.sortBy == SortByFileName) {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr1 compareByName:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByFileSize) {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr1 compareBySize:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByFileType) {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr1 compareByType:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByLastMod) {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr1 compareByLastMod:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else {
            return ^NSComparisonResult(FileResult *fr1, FileResult *fr2) {
                return [fr1 compareByPath:fr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        }
    }
}

- (NSArray<FileResult*>*) sort:(NSArray<FileResult*>*)fileResults {
    return [fileResults sortedArrayUsingComparator:[self getFileResultComparator]];
}

@end

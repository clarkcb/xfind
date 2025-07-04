#ifndef FileResultSorter_h
#define FileResultSorter_h

#import <Foundation/Foundation.h>
#import "FileResult.h"
#import "FindSettings.h"

@interface FileResultSorter : NSObject

@property FindSettings *settings;

- (instancetype) initWithSettings:(FindSettings*)settings;

- (NSComparisonResult (^)(FileResult*, FileResult*)) getFileResultComparator;

- (NSArray<FileResult*>*) sort:(NSArray<FileResult*>*)fileResults;

@end

#endif /* FileResultSorter_h */

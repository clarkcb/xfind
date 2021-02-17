#import <XCTest/XCTest.h>
#import "FileTypes.h"
#import "FindFile.h"

@interface FindFileTests : XCTestCase
//@property FileTypes *fileTypes;
@end

@implementation FindFileTests

- (void)testFindFileAbsPath {
    NSString *path = @"/Users/cary/src/xfind/objc/objcfind/objcfind/FindFile.m";
    FindFile *findFile = [[FindFile alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode];
    NSString *findFileString = [findFile description];
    XCTAssert([findFileString isEqualToString:path]);
}

- (void)testFindFileTildePath {
    NSString *path = @"~/src/xfind/objc/objcfind/objcfind/FindFile.m";
    FindFile *findFile = [[FindFile alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode];
    NSString *findFileString = [findFile description];
    XCTAssert([findFileString isEqualToString:path]);
}

- (void)testFindFileRelPath1 {
    NSString *path = @"./FindFile.m";
    FindFile *findFile = [[FindFile alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode];
    NSString *findFileString = [findFile description];
    XCTAssert([findFileString isEqualToString:path]);
}

- (void)testFindFileRelPath2 {
    NSString *path = @"../FindFile.m";
    FindFile *findFile = [[FindFile alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode];
    NSString *findFileString = [findFile description];
    XCTAssert([findFileString isEqualToString:path]);
}

@end

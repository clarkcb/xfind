#import <XCTest/XCTest.h>
#import "FileTypes.h"
#import "FileResult.h"

@interface FileResultTests : XCTestCase
//@property FileTypes *fileTypes;
@end

@implementation FileResultTests

- (void)testFileResultAbsPath {
    NSString *path = @"/Users/cary/src/xfind/objc/objcfind/objcfind/FileResult.m";
    FileResult *fileResult = [[FileResult alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode
                              stat:nil];
    NSString *fileResultString = [fileResult description];
    XCTAssert([fileResultString isEqualToString:path]);
}

- (void)testFileResultTildePath {
    NSString *path = @"~/src/xfind/objc/objcfind/objcfind/FileResult.m";
    FileResult *fileResult = [[FileResult alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode
                              stat:nil];
    NSString *fileResultString = [fileResult description];
    XCTAssert([fileResultString isEqualToString:path]);
}

- (void)testFileResultRelPath1 {
    NSString *path = @"./FileResult.m";
    FileResult *fileResult = [[FileResult alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode
                              stat:nil];
    NSString *fileResultString = [fileResult description];
    XCTAssert([fileResultString isEqualToString:path]);
}

- (void)testFileResultRelPath2 {
    NSString *path = @"../FileResult.m";
    FileResult *fileResult = [[FileResult alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode
                              stat:nil];
    NSString *fileResultString = [fileResult description];
    XCTAssert([fileResultString isEqualToString:path]);
}

@end

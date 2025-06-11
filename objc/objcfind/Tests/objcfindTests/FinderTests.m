//
//  FinderTest.m
//  objcfind_tests
//
//  Created by Cary Clark on 11/10/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "FileUtil.h"
#import "FindConfig.h"
#import "Finder.h"
#import "FindSettings.h"

@interface FinderTests : XCTestCase
@property NSString *testFilePath;
@end

@implementation FinderTests

- (void)setUp {
    [super setUp];
    self.testFilePath = [FileUtil joinPath:[NSString stringWithString:getXfindSharedPath()] childPath:@"testFiles/testFile2.txt"];
}

- (void)tearDown {
    [super tearDown];
}

/*************************************************************
 * isMatchingDir tests
 *************************************************************/
- (void)testIsMatchingDir_SingleDot_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingDir:@"."]);
}

- (void)testIsMatchingDir_DoubleDot_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingDir:@".."]);
}

- (void)testIsMatchingDir_IsHidden_False {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingDir:@".git"]);
}

- (void)testIsMatchingDir_IsHiddenIncludeHidden_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setIncludeHidden:true];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingDir:@".git"]);
}

- (void)testIsMatchingDir_NoPatterns_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingDir:@"/Users"]);
}

- (void)testIsMatchingDir_MatchesInPattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInDirPattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingDir:@"CsFind"]);
}

- (void)testIsMatchingDir_MatchesOutPattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutDirPattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingDir:@"CsFind"]);
}

- (void)testIsMatchingDir_DoesNotMatchOutPattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutDirPattern:@"FindFiles"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingDir:@"CsFind"]);
}

/*************************************************************
 * isMatchingFile tests
 *************************************************************/
- (void)testIsMatchingFile_NoExtensionsNoPatterns_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingFile:@"FileUtil.cs"]);
}

- (void)testIsMatchingFile_MatchesInExtension_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInExtension:@"cs"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingFile:@"FileUtil.cs"]);
}

- (void)testIsMatchingFile_DoesNotMatchInExtension_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInExtension:@"java"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingFile:@"FileUtil.cs"]);
}

- (void)testIsMatchingFile_MatchesOutExtension_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutExtension:@"cs"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingFile:@"FileUtil.cs"]);
}

- (void)testIsMatchingFile_DoesNotMatchOutExtension_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutExtension:@"java"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingFile:@"FileUtil.cs"]);
}

- (void)testIsMatchingFile_MatchesInFilePattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInFilePattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingFile:@"Finder.cs"]);
}

- (void)testIsMatchingFile_DoesNotMatchInFilePattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInFilePattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingFile:@"FileUtil.cs"]);
}

- (void)testIsMatchingFile_MatchesOutFilePattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutFilePattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingFile:@"Finder.cs"]);
}

- (void)testIsMatchingFile_DoesNotMatchOutFilePattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutFilePattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingFile:@"FileUtil.cs"]);
}

/*************************************************************
 * isMatchingArchiveFile tests
 *************************************************************/
- (void)testIsMatchingArchiveFile_NoExtensionsNoPatterns_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingArchiveFile:@"archive.zip"]);
}

- (void)testIsMatchingArchiveFile_MatchesInExtension_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInArchiveExtension:@"zip"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingArchiveFile:@"archive.zip"]);
}

- (void)testIsMatchingArchiveFile_DoesNotMatchInExtension_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInArchiveExtension:@"gz"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingArchiveFile:@"archive.zip"]);
}

- (void)testIsMatchingArchiveFile_MatchesOutExtension_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutArchiveExtension:@"zip"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingArchiveFile:@"archive.zip"]);
}

- (void)testIsMatchingArchiveFile_DoesNotMatchOutExtension_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutArchiveExtension:@"gz"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingArchiveFile:@"archive.zip"]);
}

- (void)testIsMatchingArchiveFile_MatchesInArchiveFilePattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInArchiveFilePattern:@"arch"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingArchiveFile:@"archive.zip"]);
}

- (void)testIsMatchingArchiveFile_DoesNotMatchInArchiveFilePattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInArchiveFilePattern:@"archives"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingArchiveFile:@"archive.zip"]);
}

- (void)testIsMatchingArchiveFile_MatchesOutArchiveFilePattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutArchiveFilePattern:@"arch"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isMatchingArchiveFile:@"archive.zip"]);
}

- (void)testIsMatchingArchiveFile_DoesNotMatchOutArchiveFilePattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutArchiveFilePattern:@"archives"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isMatchingArchiveFile:@"archive.zip"]);
}

/*************************************************************
 * filterToFileResult tests
 *************************************************************/
- (void)testFilterToFindFile_IsHidden_False {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@".gitignore" error:&error] == nil);
}

- (void)testFilterToFindFile_IsHiddenIncludeHidden_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setIncludeHidden:true];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@".hidden.txt" error:&error] != nil);
}

- (void)testFilterToFindFile_ArchiveNoFindArchives_False {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@"archive.zip" error:&error] == nil);
}

- (void)testFilterToFindFile_ArchiveFindArchives_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setIncludeArchives:true];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@"archive.zip" error:&error] != nil);
}

- (void)testFilterToFindFile_IsArchiveFindFile_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setIncludeArchives:true];
    [settings addInArchiveExtension:@"zip"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@"archive.zip" error:&error] != nil);
}

- (void)testFilterToFindFile_NotIsArchiveFindFile_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setIncludeArchives:true];
    [settings addOutArchiveExtension:@"zip"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@"archive.zip" error:&error] == nil);
}

- (void)testFilterToFindFile_ArchiveFileArchivesOnly_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setArchivesOnly:true];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@"archive.zip" error:&error] != nil);
}

- (void)testFilterToFindFile_NoExtensionsNoPatterns_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@"FileUtil.cs" error:&error] != nil);
}

- (void)testFilterToFindFile_IsFindFile_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInExtension:@"cs"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@"FileUtil.cs" error:&error] != nil);
}

- (void)testFilterToFindFile_NotIsFindFile_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutExtension:@"cs"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@"FileUtil.cs" error:&error] == nil);
}

- (void)testFilterToFindFile_NonArchiveFileArchivesOnly_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setArchivesOnly:true];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterToFileResult:@"FileUtil.cs" error:&error] == nil);
}

/*************************************************************
 * followSymlinks tests
 *************************************************************/
- (void)testFollowSymlinks_DefaultSettings_Excluded {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addPath:getXfindBinPath()];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    NSArray<FileResult*>* fileResults = [finder find:&error];
    XCTAssert([fileResults count] < 4);
}

- (void)testFollowSymlinks_FollowSymlink_Included {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addPath:getXfindBinPath()];
    [settings setFollowSymlinks:true];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    NSArray<FileResult*>* fileResults = [finder find:&error];
    XCTAssert([fileResults count] == 0 || [fileResults count] > 2);
}

- (void)testFollowSymlinks_NoFollowSymlink_Excluded {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addPath:getXfindBinPath()];
    [settings setFollowSymlinks:false];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    NSArray<FileResult*>* fileResults = [finder find:&error];
    XCTAssert([fileResults count] < 4);
}

@end

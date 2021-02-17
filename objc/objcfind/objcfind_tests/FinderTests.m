//
//  FinderTest.m
//  objcfind_tests
//
//  Created by Cary Clark on 11/10/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "FileUtil.h"
#import "Finder.h"
#import "FindSettings.h"

@interface FinderTests : XCTestCase
@property NSString *testFilePath;
@end

@implementation FinderTests

- (void)setUp {
    [super setUp];
    self.testFilePath = [FileUtil joinPath:[NSString stringWithUTF8String:SHAREDPATH] childPath:@"testFiles/testFile2.txt"];
}

- (void)tearDown {
    [super tearDown];
}

/*************************************************************
 * isFindDir tests
 *************************************************************/
- (void)testIsFindDir_SingleDot_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindDir:@"."]);
}

- (void)testIsFindDir_DoubleDot_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindDir:@".."]);
}

- (void)testIsFindDir_IsHidden_False {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isFindDir:@".git"]);
}

- (void)testIsFindDir_IsHiddenIncludeHidden_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setExcludeHidden:false];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindDir:@".git"]);
}

- (void)testIsFindDir_NoPatterns_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindDir:@"/Users"]);
}

- (void)testIsFindDir_MatchesInPattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInDirPattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindDir:@"CsFind"]);
}

- (void)testIsFindDir_MatchesOutPattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutDirPattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isFindDir:@"CsFind"]);
}

- (void)testIsFindDir_DoesNotMatchOutPattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutDirPattern:@"FindFiles"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindDir:@"CsFind"]);
}

/*************************************************************
 * isFindFile tests
 *************************************************************/
- (void)testIsFindFile_NoExtensionsNoPatterns_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindFile:@"FileUtil.cs"]);
}

- (void)testIsFindFile_MatchesInExtension_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInExtension:@"cs"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindFile:@"FileUtil.cs"]);
}

- (void)testIsFindFile_DoesNotMatchInExtension_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInExtension:@"java"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isFindFile:@"FileUtil.cs"]);
}

- (void)testIsFindFile_MatchesOutExtension_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutExtension:@"cs"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isFindFile:@"FileUtil.cs"]);
}

- (void)testIsFindFile_DoesNotMatchOutExtension_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutExtension:@"java"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindFile:@"FileUtil.cs"]);
}

- (void)testIsFindFile_MatchesInFilePattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInFilePattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindFile:@"Finder.cs"]);
}

- (void)testIsFindFile_DoesNotMatchInFilePattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInFilePattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isFindFile:@"FileUtil.cs"]);
}

- (void)testIsFindFile_MatchesOutFilePattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutFilePattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isFindFile:@"Finder.cs"]);
}

- (void)testIsFindFile_DoesNotMatchOutFilePattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutFilePattern:@"Find"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isFindFile:@"FileUtil.cs"]);
}

/*************************************************************
 * isArchiveFindFile tests
 *************************************************************/
- (void)testIsArchiveFindFile_NoExtensionsNoPatterns_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isArchiveFindFile:@"archive.zip"]);
}

- (void)testIsArchiveFindFile_MatchesInExtension_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInArchiveExtension:@"zip"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isArchiveFindFile:@"archive.zip"]);
}

- (void)testIsArchiveFindFile_DoesNotMatchInExtension_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInArchiveExtension:@"gz"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isArchiveFindFile:@"archive.zip"]);
}

- (void)testIsArchiveFindFile_MatchesOutExtension_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutArchiveExtension:@"zip"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isArchiveFindFile:@"archive.zip"]);
}

- (void)testIsArchiveFindFile_DoesNotMatchOutExtension_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutArchiveExtension:@"gz"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isArchiveFindFile:@"archive.zip"]);
}

- (void)testIsArchiveFindFile_MatchesInArchiveFilePattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInArchiveFilePattern:@"arch"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isArchiveFindFile:@"archive.zip"]);
}

- (void)testIsArchiveFindFile_DoesNotMatchInArchiveFilePattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInArchiveFilePattern:@"archives"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isArchiveFindFile:@"archive.zip"]);
}

- (void)testIsArchiveFindFile_MatchesOutArchiveFilePattern_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutArchiveFilePattern:@"arch"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder isArchiveFindFile:@"archive.zip"]);
}

- (void)testIsArchiveFindFile_DoesNotMatchOutArchiveFilePattern_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutArchiveFilePattern:@"archives"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder isArchiveFindFile:@"archive.zip"]);
}

/*************************************************************
 * FilterFile tests
 *************************************************************/
- (void)testFilterFile_IsHidden_False {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder filterFile:@".gitignore"]);
}

- (void)testFilterFile_IsHiddenIncludeHidden_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setExcludeHidden:false];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterFile:@".gitignore"]);
}

- (void)testFilterFile_ArchiveNoFindArchives_False {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder filterFile:@"archive.zip"]);
}

- (void)testFilterFile_ArchiveFindArchives_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setFindArchives:true];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterFile:@"archive.zip"]);
}

- (void)testFilterFile_IsArchiveFindFile_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setFindArchives:true];
    [settings addInArchiveExtension:@"zip"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterFile:@"archive.zip"]);
}

- (void)testFilterFile_NotIsArchiveFindFile_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setFindArchives:true];
    [settings addOutArchiveExtension:@"zip"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder filterFile:@"archive.zip"]);
}

- (void)testFilterFile_ArchiveFileArchivesOnly_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setArchivesOnly:true];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterFile:@"archive.zip"]);
}

- (void)testFilterFile_NoExtensionsNoPatterns_True {
    FindSettings *settings = [[FindSettings alloc] init];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterFile:@"FileUtil.cs"]);
}

- (void)testFilterFile_IsFindFile_True {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addInExtension:@"cs"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert([finder filterFile:@"FileUtil.cs"]);
}

- (void)testFilterFile_NotIsFindFile_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings addOutExtension:@"cs"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder filterFile:@"FileUtil.cs"]);
}

- (void)testFilterFile_NonArchiveFileArchivesOnly_False {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setArchivesOnly:true];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    XCTAssert(![finder filterFile:@"FileUtil.cs"]);
}

/*************************************************************
 * findMultiLineString tests
 *************************************************************/
- (void)testFindMultiLineString {
    FindSettings *settings = [[FindSettings alloc] init];
    [settings setStartPath:@"."];
    [settings addFindPattern:@"Finder"];
    NSError *error = nil;
    Finder *finder = [[Finder alloc] initWithSettings:settings error:&error];
    
    NSArray<FindResult*> *results = [finder findFilePath:self.testFilePath error:&error];

    XCTAssert([results count] == 2);

    FindResult *firstResult = [results objectAtIndex:0];
    int expectedFirstLineNum = 29;
    XCTAssert([firstResult lineNum] == expectedFirstLineNum);
    int expectedFirstMatchStartIndex = 3;
    XCTAssert([firstResult matchStartIndex] == expectedFirstMatchStartIndex);
    int expectedFirstMatchEndIndex = 11;
    XCTAssert([firstResult matchEndIndex] == expectedFirstMatchEndIndex);

    FindResult *secondResult = [results objectAtIndex:1];
    int expectedSecondLineNum = 35;
    XCTAssert([secondResult lineNum] == expectedSecondLineNum);
    int expectedSecondMatchStartIndex = 24;
    XCTAssert([secondResult matchStartIndex] == expectedSecondMatchStartIndex);
    int expectedSecondMatchEndIndex = 32;
    XCTAssert([secondResult matchEndIndex] == expectedSecondMatchEndIndex);
}

@end

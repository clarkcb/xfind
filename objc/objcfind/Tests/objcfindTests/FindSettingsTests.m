//
//  FindSettingsTests.m
//  objcfind_tests
//
//  Created by Cary Clark on 11/12/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "FindSettings.h"

@interface FindSettingsTests : XCTestCase

@end

@implementation FindSettingsTests

- (void)setUp {
    [super setUp];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testDefaultSettings {
    FindSettings *settings = [[FindSettings alloc] init];
    XCTAssert(![settings archivesOnly]);
    XCTAssert(![settings debug]);
    XCTAssert(![settings includeArchives]);
    XCTAssert(![settings includeHidden]);
    XCTAssert(![settings printDirs]);
    XCTAssert(![settings printFiles]);
    XCTAssert(![settings printUsage]);
    XCTAssert(![settings printVersion]);
    XCTAssert(![settings verbose]);
}

- (void)testAddExtensions {
    FindSettings *settings = [[FindSettings alloc] init];
    XCTAssert([[settings inExtensions] count] == 0);
    [settings addInExtension:@"java,scala"];
    XCTAssert([[settings inExtensions] count] == 2);
    XCTAssert([[[settings inExtensions] objectAtIndex:0] isEqual:@"java"]);
    XCTAssert([[[settings inExtensions] objectAtIndex:1] isEqual:@"scala"]);
}

- (void)testAddPattern {
    FindSettings *settings = [[FindSettings alloc] init];
    XCTAssert([[settings inFilePatterns] count] == 0);
    [settings addInFilePattern:@"Finder"];
    XCTAssert([[settings inFilePatterns] count] == 1);
    XCTAssert([[[[settings inFilePatterns] objectAtIndex:0] pattern] isEqual:@"Finder"]);
}

- (void)testSetArchivesOnly {
    FindSettings *settings = [[FindSettings alloc] init];
    XCTAssert(![settings archivesOnly]);
    XCTAssert(![settings includeArchives]);
    [settings setArchivesOnly:true];
    XCTAssert([settings archivesOnly]);
    XCTAssert([settings includeArchives]);
}

- (void)testSetDebug {
    FindSettings *settings = [[FindSettings alloc] init];
    XCTAssert(![settings debug]);
    XCTAssert(![settings verbose]);
    [settings setDebug:true];
    XCTAssert([settings debug]);
    XCTAssert([settings verbose]);
}

@end

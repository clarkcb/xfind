//
//  FindOptionsTests.m
//  objcfind_tests
//
//  Created by Cary Clark on 11/11/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "FileUtil.h"
#import "FindOptions.h"

@interface FindOptionsTests : XCTestCase

@end

@implementation FindOptionsTests

- (void)setUp {
    [super setUp];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testSettingsFromMinimalArgs {
    FindOptions *options = [[FindOptions alloc] init];
    NSArray *args =[NSArray arrayWithObjects:@"objfind",@"-s",@"Finder",@".",nil];
    NSError *error = nil;
    FindSettings *settings = [options settingsFromArgs:args error:&error];
    XCTAssert(![settings archivesOnly]);
    XCTAssert(![settings debug]);
    XCTAssert([settings excludeHidden]);
    XCTAssert(![settings firstMatch]);
    XCTAssert(![settings listDirs]);
    XCTAssert(![settings listFiles]);
    XCTAssert(![settings listLines]);
    XCTAssert(![settings multiLineFind]);
    XCTAssert([settings printResults]);
    XCTAssert(![settings printUsage]);
    XCTAssert(![settings printVersion]);
    XCTAssert(![settings findArchives]);
    XCTAssert(![settings uniqueLines]);
    XCTAssert(![settings verbose]);
    
    XCTAssert([[settings findPatterns] count] == 1);
    XCTAssert([[[[settings findPatterns] objectAtIndex:0] pattern] isEqual:@"Finder"]);
    XCTAssert([[settings startPath] isEqual:@"."]);
}

- (void)testSettingsFromValidArgs {
    FindOptions *options = [[FindOptions alloc] init];
    NSArray *args =[NSArray arrayWithObjects:@"objfind",@"-x",@"java,scala",@"-s",@"Find",@".",nil];
    NSError *error = nil;
    FindSettings *settings = [options settingsFromArgs:args error:&error];
    
    XCTAssert([[settings inExtensions] count] == 2);
    XCTAssert([[[settings inExtensions] objectAtIndex:0] isEqual:@"java"]);
    XCTAssert([[[settings inExtensions] objectAtIndex:1] isEqual:@"scala"]);
    XCTAssert([[settings findPatterns] count] == 1);
    XCTAssert([[[[settings findPatterns] objectAtIndex:0] pattern] isEqual:@"Find"]);
    XCTAssert([[settings startPath] isEqual:@"."]);
}

- (void)testSettingsFromJson {
    FindSettings *settings = [[FindSettings alloc] init];

    NSString *startPath = @"~/src/xfind";
    NSString *json = [NSString stringWithFormat:@"{\n"
                      "\"startpath\": \"%@\",\n"
                      "\"in-ext\": [\"js\", \"ts\"],\n"
                      "\"out-dirpattern\": \"node_module\",\n"
                      "\"out-filepattern\": [\"temp\"],\n"
                      "\"findpattern\": \"Finder\",\n"
                      "\"linesbefore\": 2,\n"
                      "\"linesafter\": 2,\n"
                      "\"debug\": true,\n"
                      "\"allmatches\": false,\n"
                      "\"includehidden\": false\n"
                      "}", startPath];

    NSData *data = [json dataUsingEncoding:NSUTF8StringEncoding];

    FindOptions *options = [[FindOptions alloc] init];
    [options settingsFromData:data settings:settings];

    XCTAssert([[settings inExtensions] count] == 2);
    XCTAssert([[[settings inExtensions] objectAtIndex:0] isEqual:@"js"]);
    XCTAssert([[[settings inExtensions] objectAtIndex:1] isEqual:@"ts"]);
    XCTAssert([[settings outDirPatterns] count] == 1);
    XCTAssert([[[[settings outDirPatterns] objectAtIndex:0] pattern] isEqual:@"node_module"]);
    XCTAssert([[settings outFilePatterns] count] == 1);
    XCTAssert([[[[settings outFilePatterns] objectAtIndex:0] pattern] isEqual:@"temp"]);
    XCTAssert([[settings findPatterns] count] == 1);
    XCTAssert([[[[settings findPatterns] objectAtIndex:0] pattern] isEqual:@"Finder"]);
    XCTAssert([settings linesBefore] == 2);
    XCTAssert([settings linesAfter] == 2);
    XCTAssert([settings debug]);
    XCTAssert([settings firstMatch]);
    XCTAssert([settings excludeHidden]);
    //NSString *sp = [settings startPath];
    //XCTAssert([[settings startPath] isEqual:startPath]);
}

@end

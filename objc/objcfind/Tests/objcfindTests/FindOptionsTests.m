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
    NSArray *args =[NSArray arrayWithObjects:@"objfind",@".",nil];
    NSError *error = nil;
    FindSettings *settings = [options settingsFromArgs:args error:&error];
    XCTAssert(![settings archivesOnly]);
    XCTAssert(![settings debug]);
    XCTAssert(![settings followSymlinks]);
    XCTAssert(![settings includeArchives]);
    XCTAssert(![settings includeHidden]);
    XCTAssert(![settings printDirs]);
    XCTAssert([settings printFiles]);
    XCTAssert(![settings printUsage]);
    XCTAssert(![settings printVersion]);
    XCTAssert(![settings verbose]);
    
    XCTAssert([[settings paths] count] == 1);
    XCTAssert([[[settings paths] objectAtIndex:0] isEqual:@"."]);
}

- (void)testSettingsFromValidArgs {
    FindOptions *options = [[FindOptions alloc] init];
    NSArray *args =[NSArray arrayWithObjects:@"objfind",@"-x",@"java,scala",@".",nil];
    NSError *error = nil;
    FindSettings *settings = [options settingsFromArgs:args error:&error];
    
    XCTAssert([[settings inExtensions] count] == 2);
    XCTAssert([[[settings inExtensions] objectAtIndex:0] isEqual:@"java"]);
    XCTAssert([[[settings inExtensions] objectAtIndex:1] isEqual:@"scala"]);
    XCTAssert([[settings paths] count] == 1);
    XCTAssert([[[settings paths] objectAtIndex:0] isEqual:@"."]);
}

- (void)testSettingsFromJson {
    FindSettings *settings = [[FindSettings alloc] init];

    NSString *startPath = @"~/src/xfind";
    NSString *json = [NSString stringWithFormat:@"{\n"
                      "\"path\": \"%@\",\n"
                      "\"in-ext\": [\"js\", \"ts\"],\n"
                      "\"out-dirpattern\": \"node_module\",\n"
                      "\"out-filepattern\": [\"temp\"],\n"
                      "\"debug\": true,\n"
                      "\"followsymlinks\": true,\n"
                      "\"includehidden\": true\n"
                      "}", startPath];

    NSData *data = [json dataUsingEncoding:NSUTF8StringEncoding];

    FindOptions *options = [[FindOptions alloc] init];
    NSError *error = nil;
    [options settingsFromData:data settings:settings error:&error];

    XCTAssert([[settings inExtensions] count] == 2);
    XCTAssert([[[settings inExtensions] objectAtIndex:0] isEqual:@"js"]);
    XCTAssert([[[settings inExtensions] objectAtIndex:1] isEqual:@"ts"]);
    XCTAssert([[settings outDirPatterns] count] == 1);
    XCTAssert([[[[settings outDirPatterns] objectAtIndex:0] pattern] isEqual:@"node_module"]);
    XCTAssert([[settings outFilePatterns] count] == 1);
    XCTAssert([[[[settings outFilePatterns] objectAtIndex:0] pattern] isEqual:@"temp"]);
    XCTAssert([[settings paths] count] == 1);
    XCTAssert([[[settings paths] objectAtIndex:0] isEqual:@"~/src/xfind"]);
    XCTAssert([settings debug]);
    XCTAssert([settings verbose]);
    XCTAssert([settings followSymlinks]);
    XCTAssert([settings includeHidden]);
}

@end

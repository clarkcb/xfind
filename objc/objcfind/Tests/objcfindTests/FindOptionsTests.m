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
    NSError *error = nil;
    FindConfig *config = [[FindConfig alloc] init];
    FindOptions *options = [[FindOptions alloc] initWithConfig:config error:&error];
    NSArray *args =[NSArray arrayWithObjects:@"objfind",@".",nil];
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
    NSError *error = nil;
    FindConfig *config = [[FindConfig alloc] init];
    FindOptions *options = [[FindOptions alloc] initWithConfig:config error:&error];
    NSArray *args =[NSArray arrayWithObjects:@"objfind",@"-x",@"java,scala",@".",nil];
    FindSettings *settings = [options settingsFromArgs:args error:&error];
    
    XCTAssert([[settings inExtensions] count] == 2);
    XCTAssert([[[settings inExtensions] objectAtIndex:0] isEqual:@"java"]);
    XCTAssert([[[settings inExtensions] objectAtIndex:1] isEqual:@"scala"]);
    XCTAssert([[settings paths] count] == 1);
    XCTAssert([[[settings paths] objectAtIndex:0] isEqual:@"."]);
}

- (void)testSettingsFromJson {
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

    NSError *error = nil;
    FindConfig *config = [[FindConfig alloc] init];
    FindOptions *options = [[FindOptions alloc] initWithConfig:config error:&error];
    FindSettings *settings = [options settingsFromData:data error:&error];

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

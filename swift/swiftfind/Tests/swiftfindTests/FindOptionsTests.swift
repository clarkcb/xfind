//
//  FindOptionsTests.swift
//  swiftfind
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import XCTest

import swiftfind

class FindOptionsTests: XCTestCase {
    let options = FindOptions()
    let startPath: String = "."
    var requiredArgs: [String] = []

    override func setUp() {
        super.setUp()
        requiredArgs = [startPath]
    }

//    override func tearDown() {
//        super.tearDown()
//    }

    func testSettingsEqualDefaultFindSettings() {
        let settings: FindSettings = try! options.settingsFromArgs(requiredArgs)
        XCTAssert(settings.archivesOnly == DefaultFindSettings.archivesOnly, "archivesOnly == false")
        XCTAssert(settings.debug == DefaultFindSettings.debug, "debug == false")
        XCTAssert(settings.followSymlinks == DefaultFindSettings.followSymlinks, "followSymlinks == false")
        XCTAssert(settings.includeHidden == DefaultFindSettings.includeHidden, "includeHidden == false")
        XCTAssert(settings.printDirs == DefaultFindSettings.printDirs, "printDirs == false")
        XCTAssert(settings.printFiles == true, "printFiles == true")
        XCTAssert(settings.printUsage == DefaultFindSettings.printUsage, "printUsage == false")
        XCTAssert(settings.printVersion == DefaultFindSettings.printVersion, "printVersion == false")
        XCTAssert(settings.includeArchives == DefaultFindSettings.includeArchives,
                  "includeArchives == false")
        XCTAssert(settings.verbose == DefaultFindSettings.verbose, "verbose == false")
    }

    func testSettingsFromArgs() {
        let otherArgs: [String] = ["--in-ext", "scala,swift", "--debug"]
        let settings: FindSettings = try! options.settingsFromArgs(requiredArgs + otherArgs)
        print("settings: \(settings)")
        XCTAssert(settings.debug, "debug == true")
        XCTAssert(settings.verbose, "verbose == true")
        XCTAssertEqual(2, settings.inExtensions.count)
        XCTAssertTrue(settings.inExtensions.contains("scala"))
        XCTAssertTrue(settings.inExtensions.contains("swift"))
        XCTAssertEqual(1, settings.paths.count)
        XCTAssertTrue(settings.paths.contains(startPath))
    }

    func testSettingsFromJson() {
        let jsonString = """
        {
          "path": "~/src/xfind/",
          "in-ext": ["js", "ts"],
          "out-dirpattern": ["_", "ansible", "bak", "build", "chef", "node_module", "target", "test", "typings"],
          "out-filepattern": ["gulpfile", ".min."],
          "debug": true,
          "followsymlinks": true,
          "includehidden": true,
          "printdirs": true,
          "printfiles": true
        }
        """
        let settings = try! options.settingsFromJson(jsonString)
        print("settings: \(settings)")
        XCTAssertTrue(settings.debug, "debug == true")
        XCTAssertTrue(settings.followSymlinks, "followSymlinks == true")
        XCTAssertTrue(settings.includeHidden, "includeHidden == true")
        XCTAssertEqual(2, settings.inExtensions.count)
        XCTAssertTrue(settings.inExtensions.contains("js"))
        XCTAssertTrue(settings.inExtensions.contains("ts"))
        XCTAssertEqual(9, settings.outDirPatterns.count)
        XCTAssertEqual(1, settings.paths.count)
        XCTAssertTrue(settings.paths.contains("~/src/xfind/"))
        XCTAssertTrue(settings.printDirs)
        XCTAssertTrue(settings.printFiles)
        XCTAssertTrue(settings.verbose, "verbose == true")
    }

    static var allTests = [
        ("testSettingsEqualDefaultFindSettings", testSettingsEqualDefaultFindSettings),
        ("testSettingsFromArgs", testSettingsFromArgs),
        ("testSettingsFromJson", testSettingsFromJson)
    ]
}

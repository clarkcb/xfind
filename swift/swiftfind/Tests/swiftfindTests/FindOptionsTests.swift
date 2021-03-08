//
//  FindOptionsTests.swift
//  swiftfind
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Cocoa
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

    override func tearDown() {
        super.tearDown()
    }

    func testSettingsEqualDefaultSettings() {
        var error: NSError?
        let settings: FindSettings = options.settingsFromArgs(requiredArgs, error: &error)
        XCTAssert(settings.archivesOnly == DefaultSettings.archivesOnly, "archivesOnly == false")
        XCTAssert(settings.debug == DefaultSettings.debug, "debug == false")
        XCTAssert(settings.excludeHidden == DefaultSettings.excludeHidden, "excludeHidden == true")
        XCTAssert(settings.listDirs == DefaultSettings.listDirs, "listDirs == false")
        XCTAssert(settings.listFiles == true, "listFiles == true")
        XCTAssert(settings.printUsage == DefaultSettings.printUsage, "printUsage == false")
        XCTAssert(settings.printVersion == DefaultSettings.printVersion, "printVersion == false")
        XCTAssert(settings.includeArchives == DefaultSettings.includeArchives,
                  "includeArchives == false")
        XCTAssert(settings.verbose == DefaultSettings.verbose, "verbose == false")
    }

    func testSettingsFromArgs() {
        var error: NSError?
        let otherArgs: [String] = ["--in-ext", "scala,swift", "--debug"]
        let settings: FindSettings = options.settingsFromArgs(requiredArgs + otherArgs, error: &error)
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
        var error: NSError?
        let jsonString = """
{
  "path": "~/src/xfind/",
  "in-ext": ["js", "ts"],
  "out-dirpattern": ["_", "ansible", "bak", "build", "chef", "node_module", "target", "test", "typings"],
  "out-filepattern": ["gulpfile", ".min."],
  "debug": true,
  "includehidden": false,
  "listdirs": true,
  "listfiles": true
}
"""
        let settings = options.settingsFromJson(jsonString, error: &error)
        print("settings: \(settings)")
        XCTAssertTrue(settings.debug, "debug == true")
        XCTAssertTrue(settings.excludeHidden, "excludeHidden == true")
        XCTAssertEqual(2, settings.inExtensions.count)
        XCTAssertTrue(settings.inExtensions.contains("js"))
        XCTAssertTrue(settings.inExtensions.contains("ts"))
        XCTAssertTrue(settings.listDirs)
        XCTAssertTrue(settings.listFiles)
        XCTAssertEqual(9, settings.outDirPatterns.count)
        XCTAssertEqual(1, settings.paths.count)
        XCTAssertTrue(settings.paths.contains("~/src/xfind/"))
        XCTAssertTrue(settings.verbose, "verbose == true")
    }
}

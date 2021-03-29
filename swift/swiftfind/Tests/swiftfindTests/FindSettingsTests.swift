//
//  FindSettingsTests.swift
//  swiftfind
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import XCTest

import swiftfind

class FindSettingsTests: XCTestCase {
    override func setUp() {
        super.setUp()
    }

    override func tearDown() {
        super.tearDown()
    }

    func testDefaultSettings() {
        XCTAssert(DefaultSettings.archivesOnly == false, "archivesOnly == false")
        XCTAssert(DefaultSettings.colorize == true, "colorize == true")
        XCTAssert(DefaultSettings.debug == false, "debug == false")
        XCTAssert(DefaultSettings.excludeHidden == true, "excludeHidden == true")
        XCTAssert(DefaultSettings.includeArchives == false, "includeArchives == false")
        XCTAssert(DefaultSettings.listDirs == false, "listDirs == false")
        XCTAssert(DefaultSettings.listFiles == false, "listFiles == false")
        XCTAssert(DefaultSettings.printUsage == false, "printUsage == false")
        XCTAssert(DefaultSettings.printVersion == false, "printVersion == false")
        XCTAssert(DefaultSettings.verbose == false, "verbose == false")
    }

    func testInitialSettingsEqualDefaultSettings() {
        let settings = FindSettings()
        XCTAssert(settings.archivesOnly == DefaultSettings.archivesOnly, "archivesOnly == false")
        XCTAssert(settings.colorize == DefaultSettings.colorize, "colorize == true")
        XCTAssert(settings.debug == DefaultSettings.debug, "debug == false")
        XCTAssert(settings.excludeHidden == DefaultSettings.excludeHidden, "excludeHidden == true")
        XCTAssert(settings.includeArchives == DefaultSettings.includeArchives,
                  "includeArchives == false")
        XCTAssert(settings.listDirs == DefaultSettings.listDirs, "listDirs == false")
        XCTAssert(settings.listFiles == DefaultSettings.listFiles, "listFiles == false")
        XCTAssert(settings.printUsage == DefaultSettings.printUsage, "printUsage == false")
        XCTAssert(settings.printVersion == DefaultSettings.printVersion, "printVersion == false")
        XCTAssert(settings.verbose == DefaultSettings.verbose, "verbose == false")
    }

    func testAddExtensions() {
        let settings = FindSettings()
        settings.addInExtension("java")
        settings.addInExtension("scala")
        settings.addInExtension("cs,fs")
        XCTAssert(settings.inExtensions.count == 4)
        XCTAssert(settings.inExtensions.contains("java"))
        XCTAssert(settings.inExtensions.contains("scala"))
        XCTAssert(settings.inExtensions.contains("cs"))
        XCTAssert(settings.inExtensions.contains("fs"))
    }

    func testAddPattern() {
        let settings = FindSettings()
        settings.addInFilePattern("Finder")
        XCTAssert(settings.inFilePatterns.count == 1)
        XCTAssert(settings.inFilePatterns.contains(where: { $0.test("Finder") }))
    }

    func testSetArchivesOnly() {
        let settings = FindSettings()
        XCTAssertFalse(settings.archivesOnly)
        XCTAssertFalse(settings.includeArchives)
        settings.archivesOnly = true
        XCTAssertTrue(settings.archivesOnly)
        XCTAssertTrue(settings.includeArchives)
    }

    func testSetDebug() {
        let settings = FindSettings()
        XCTAssertFalse(settings.debug)
        XCTAssertFalse(settings.verbose)
        settings.debug = true
        XCTAssertTrue(settings.debug)
        XCTAssertTrue(settings.verbose)
    }

    static var allTests = [
        ("testDefaultSettings", testDefaultSettings),
        ("testInitialSettingsEqualDefaultSettings", testInitialSettingsEqualDefaultSettings),
        ("testAddExtensions", testAddExtensions),
        ("testAddPattern", testAddPattern),
        ("testSetArchivesOnly", testSetArchivesOnly),
        ("testSetDebug", testSetDebug),
    ]
}

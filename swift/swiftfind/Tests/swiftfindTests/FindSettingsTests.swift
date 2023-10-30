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

    func testDefaultFindSettings() {
        XCTAssert(DefaultFindSettings.archivesOnly == false, "archivesOnly == false")
        XCTAssert(DefaultFindSettings.debug == false, "debug == false")
        XCTAssert(DefaultFindSettings.includeArchives == false, "includeArchives == false")
        XCTAssert(DefaultFindSettings.includeHidden == false, "includeHidden == false")
        XCTAssert(DefaultFindSettings.listDirs == false, "listDirs == false")
        XCTAssert(DefaultFindSettings.listFiles == false, "listFiles == false")
        XCTAssert(DefaultFindSettings.maxLastMod == nil, "maxLastMod == nil")
        XCTAssert(DefaultFindSettings.maxSize == 0, "maxSize == 0")
        XCTAssert(DefaultFindSettings.minLastMod == nil, "minLastMod == nil")
        XCTAssert(DefaultFindSettings.minSize == 0, "minSize == 0")
        XCTAssert(DefaultFindSettings.printUsage == false, "printUsage == false")
        XCTAssert(DefaultFindSettings.printVersion == false, "printVersion == false")
        XCTAssert(DefaultFindSettings.sortCaseInsensitive == false, "sortCaseInsensitive == false")
        XCTAssert(DefaultFindSettings.sortDescending == false, "sortDescending == false")
        XCTAssert(DefaultFindSettings.verbose == false, "verbose == false")
    }

    func testInitialSettingsEqualDefaultFindSettings() {
        let settings = FindSettings()
        XCTAssert(settings.archivesOnly == DefaultFindSettings.archivesOnly, "archivesOnly == false")
        XCTAssert(settings.debug == DefaultFindSettings.debug, "debug == false")
        XCTAssert(settings.includeArchives == DefaultFindSettings.includeArchives,
                  "includeArchives == false")
        XCTAssert(settings.includeHidden == DefaultFindSettings.includeHidden, "includeHidden == false")
        XCTAssert(settings.listDirs == DefaultFindSettings.listDirs, "listDirs == false")
        XCTAssert(settings.listFiles == DefaultFindSettings.listFiles, "listFiles == false")
        XCTAssert(settings.maxLastMod == DefaultFindSettings.maxLastMod, "maxLastMod == nil")
        XCTAssert(settings.maxSize == DefaultFindSettings.maxSize, "maxSize == 0")
        XCTAssert(settings.minLastMod == DefaultFindSettings.minLastMod, "minLastMod == nil")
        XCTAssert(settings.minSize == DefaultFindSettings.minSize, "minSize == 0")
        XCTAssert(settings.printUsage == DefaultFindSettings.printUsage, "printUsage == false")
        XCTAssert(settings.printVersion == DefaultFindSettings.printVersion, "printVersion == false")
        XCTAssert(settings.sortCaseInsensitive == DefaultFindSettings.sortCaseInsensitive, "sortCaseInsensitive == false")
        XCTAssert(settings.sortDescending == DefaultFindSettings.sortDescending, "sortDescending == false")
        XCTAssert(settings.verbose == DefaultFindSettings.verbose, "verbose == false")
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
        ("testDefaultFindSettings", testDefaultFindSettings),
        ("testInitialSettingsEqualDefaultFindSettings", testInitialSettingsEqualDefaultFindSettings),
        ("testAddExtensions", testAddExtensions),
        ("testAddPattern", testAddPattern),
        ("testSetArchivesOnly", testSetArchivesOnly),
        ("testSetDebug", testSetDebug),
    ]
}

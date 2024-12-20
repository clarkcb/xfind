//
//  FileUtilTests.swift
//  swiftfind
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

// import Cocoa
import Foundation
import XCTest

import swiftfind

class FileUtilTests: XCTestCase {
//    override func setUp() {
//        super.setUp()
//    }
//
//    override func tearDown() {
//        super.tearDown()
//    }

    /* ==========================================================================
     * contractPath tests
     ========================================================================= */
    func testContractPathWithHome() {
        let homePath = NSHomeDirectory()
        let fullPath = FileUtil.joinPath(homePath, childPath: "filename.txt")
        let expected = "~/filename.txt"
        XCTAssertEqual(expected, FileUtil.contractPath(fullPath))
    }

    /* ==========================================================================
     * expandPath tests
     ========================================================================= */
    func testExpandPathIsTilde() {
        let expected = NSHomeDirectory()
        XCTAssertEqual(expected, FileUtil.expandPath("~"))
    }

    func testExpandPathHasTilde() {
        let homePath = NSHomeDirectory()
        let expected = FileUtil.joinPath(homePath, childPath: "filename.txt")
        XCTAssertEqual(expected, FileUtil.expandPath("~/filename.txt"))
    }

    func testExpandPathHasTildeAndName() {
        let homePath = NSHomeDirectory()
        let expected = FileUtil.joinPath(homePath, childPath: "filename.txt")
        XCTAssertEqual(expected, FileUtil.expandPath("~cary/filename.txt"))
    }

    func testExpandPathNoTilde() {
        let expected = "/path/to/filename.txt"
        XCTAssertEqual(expected, FileUtil.expandPath("/path/to/filename.txt"))
    }

    /* ==========================================================================
     * exist tests
     ========================================================================= */
    func testExistsExistingFile() {
        let config = FindConfig()
        let fileTypesFile = FileUtil.joinPath(config.sharedPath, childPath: "filetypes.json")
        XCTAssertTrue(FileUtil.exists(fileTypesFile))
    }

    func testExistsNonexistingFile() {
        let config = FindConfig()
        let fileTypesFile = FileUtil.joinPath(config.sharedPath, childPath: "filetypes.ZZZ")
        XCTAssertFalse(FileUtil.exists(fileTypesFile))
    }

    /* ==========================================================================
     * getExtension tests
     ========================================================================= */
    func testGetExtension() {
        XCTAssert(FileUtil.getExtension("filename.txt") == "txt",
                  "getExtension(\"filename.txt\") == \"txt\"")
        XCTAssert(FileUtil.getExtension("filename.TXT") == "txt",
                  "getExtension(\"filename.TXT\") == \"txt\"")
        XCTAssert(FileUtil.getExtension("filename.") == "", "getExtension(\"filename.\") == \"\"")
        XCTAssert(FileUtil.getExtension("filename") == "", "getExtension(\"filename\") == \"\"")
        XCTAssert(FileUtil.getExtension(".hidden.txt") == "txt",
                  "getExtension(\".hidden.txt\") == \"txt\"")
        XCTAssert(FileUtil.getExtension(".hidden.") == "", "getExtension(\".hidden.\") == \"\"")
        XCTAssert(FileUtil.getExtension(".hidden") == "", "getExtension(\".hidden\") == \"\"")
    }

    func testHasExtension() {
        XCTAssert(FileUtil.hasExtension("filename.txt", ext: "txt"),
                  "hasExtension(\"filename.txt\", \"txt\") == true")
        XCTAssert(FileUtil.hasExtension("filename.TXT", ext: "txt"),
                  "hasExtension(\"filename.TXT\", \"txt\") == true")
    }

    /* ==========================================================================
     * isDirectory tests
     ========================================================================= */
    func testIsDirectorySingleDot() {
        XCTAssertTrue(FileUtil.isDirectory("."))
    }

    func testIsDirectorySingleDotSlash() {
        XCTAssertTrue(FileUtil.isDirectory("./"))
    }

    func testIsDirectoryDoubleDot() {
        XCTAssertTrue(FileUtil.isDirectory(".."))
    }

    func testIsDirectoryDoubleDotSlash() {
        XCTAssertTrue(FileUtil.isDirectory("../"))
    }

    func testIsDirectoryRootDir() {
        XCTAssertTrue(FileUtil.isDirectory("/"))
    }

    func testIsDirectoryTildeHomeDir() {
        XCTAssertTrue(FileUtil.isDirectory("~"))
    }

    func testIsDirectoryNonDirectory() {
        XCTAssertFalse(FileUtil.isDirectory("filename.txt"))
    }

    /* ==========================================================================
     * isDotDir tests
     ========================================================================= */
    func testIsDotDirSingleDot() {
        let filename = "."
        XCTAssertTrue(FileUtil.isDotDir(filename))
    }

    func testIsDotDirDoubleDot() {
        let filename = ".."
        XCTAssertTrue(FileUtil.isDotDir(filename))
    }

    func testIsDotDirNotDotDir() {
        let filename = "~/path"
        XCTAssertFalse(FileUtil.isDotDir(filename))
    }

    func testIsDotDirPathWithDot() {
        let filename = "./path"
        XCTAssertFalse(FileUtil.isDotDir(filename))
    }

    func testIsDotDirHiddenFile() {
        let filename = ".gitignore"
        XCTAssertFalse(FileUtil.isDotDir(filename))
    }

    /* ==========================================================================
     * isHidden tests
     ========================================================================= */
    func testIsHiddenSingleDot() {
        let filename = "."
        XCTAssertFalse(FileUtil.isHidden(filename))
    }

    func testIsHiddenDoubleDot() {
        let filename = ".."
        XCTAssertFalse(FileUtil.isHidden(filename))
    }

    func testIsHiddenHiddenFileName() {
        let filename = ".gitignore"
        XCTAssertTrue(FileUtil.isHidden(filename))
    }

    func testIsHiddenNotHiddenFileName() {
        let filename = "./file.txt"
        XCTAssertFalse(FileUtil.isHidden(filename))
    }

    /* ==========================================================================
     * joinPath tests
     ========================================================================= */

    func testJoinPathDir() {
        let joined = FileUtil.joinPath("./path/to", childPath: "somewhere")
        XCTAssertEqual(joined, "./path/to/somewhere")
    }

    func testJoinPathFile() {
        let joined = FileUtil.joinPath("./path/to", childPath: "somefile.txt")
        XCTAssertEqual(joined, "./path/to/somefile.txt")
    }

    /* ==========================================================================
     * splitPath tests
     ========================================================================= */

    func testSplitPathDir() {
        let path = "./path/to/somewhere/"
        let (parent, child) = FileUtil.splitPath(path)
        XCTAssertEqual(parent, "./path/to")
        XCTAssertEqual(child, "somewhere")
    }

    func testSplitPathFile() {
        let path = "./path/to/somefile.txt"
        let (parent, child) = FileUtil.splitPath(path)
        XCTAssertEqual(parent, "./path/to")
        XCTAssertEqual(child, "somefile.txt")
    }

    static let allTests = [
        ("testContractPathWithHome", testContractPathWithHome),
        ("testExpandPathHasTilde", testExpandPathHasTilde),
        ("testExpandPathNoTilde", testExpandPathNoTilde),
        ("testExistsExistingFile", testExistsExistingFile),
        ("testExistsNonexistingFile", testExistsNonexistingFile),
        ("testGetExtension", testGetExtension),
        ("testHasExtension", testHasExtension),
        ("testIsDirectorySingleDot", testIsDirectorySingleDot),
        ("testIsDirectorySingleDotSlash", testIsDirectorySingleDotSlash),
        ("testIsDirectoryDoubleDot", testIsDirectoryDoubleDot),
        ("testIsDirectoryDoubleDotSlash", testIsDirectoryDoubleDotSlash),
        ("testIsDirectoryRootDir", testIsDirectoryRootDir),
        ("testIsDirectoryTildeHomeDir", testIsDirectoryTildeHomeDir),
        ("testIsDirectoryNonDirectory", testIsDirectoryNonDirectory),
        ("testIsDotDirSingleDot", testIsDotDirSingleDot),
        ("testIsDotDirDoubleDot", testIsDotDirDoubleDot),
        ("testIsDotDirNotDotDir", testIsDotDirNotDotDir),
        ("testIsDotDirPathWithDot", testIsDotDirPathWithDot),
        ("testIsDotDirHiddenFile", testIsDotDirHiddenFile),
        ("testIsHiddenSingleDot", testIsHiddenSingleDot),
        ("testIsHiddenDoubleDot", testIsHiddenDoubleDot),
        ("testIsHiddenHiddenFileName", testIsHiddenHiddenFileName),
        ("testIsHiddenNotHiddenFileName", testIsHiddenNotHiddenFileName),
        ("testJoinPathDir", testJoinPathDir),
        ("testJoinPathFile", testJoinPathFile),
        ("testSplitPathDir", testSplitPathDir),
        ("testSplitPathFile", testSplitPathFile)
    ]
}

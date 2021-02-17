//
//  swiftfindTests.swift
//  swiftfindTests
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Cocoa
import XCTest

import swiftfind

class FindFileTests: XCTestCase {

    func testFindFileAbsPath() {
        let path = "/Users/cary/src/xfind/swift/swiftfind/Sources/swiftfind/FindFile.swift"
        let findFile = FindFile(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, findFile.description)
    }

    func testFindFileTildePath() {
        let path = "~/src/xfind/swift/swiftfind/Sources/swiftfind/FindFile.swift"
        let findFile = FindFile(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, findFile.description)
    }

    func testFindFileRelPath1() {
        let path = "./FindFile.swift"
        let findFile = FindFile(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, findFile.description)
    }

    func testFindFileRelPath2() {
        let path = "../FindFile.swift"
        let findFile = FindFile(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, findFile.description)
    }
}

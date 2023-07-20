//
//  FileResultTests.swift
//  swiftfindTests
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import XCTest

import swiftfind

class FileResultTests: XCTestCase {

    func testFileResultAbsPath() {
        let filePath = "\(NSHomeDirectory())/src/xfind/swift/swiftfind/Sources/swiftfind/FileResult.swift"
        let fileResult = FileResult(filePath: filePath, fileType: FileType.code)
        XCTAssertEqual(filePath, fileResult.description)
    }

    func testFileResultTildePath() {
        let filePath = "~/src/xfind/swift/swiftfind/Sources/swiftfind/FileResult.swift"
        let fileResult = FileResult(filePath: filePath, fileType: FileType.code)
        XCTAssertEqual(filePath, fileResult.description)
    }

    func testFileResultRelPath1() {
        let filePath = "./FileResult.swift"
        let fileResult = FileResult(filePath: filePath, fileType: FileType.code)
        XCTAssertEqual(filePath, fileResult.description)
    }

    func testFileResultRelPath2() {
        let filePath = "../FileResult.swift"
        let fileResult = FileResult(filePath: filePath, fileType: FileType.code)
        XCTAssertEqual(filePath, fileResult.description)
    }

    static var allTests = [
        ("testFileResultAbsPath", testFileResultAbsPath),
        ("testFileResultTildePath", testFileResultTildePath),
        ("testFileResultRelPath1", testFileResultRelPath1),
        ("testFileResultRelPath2", testFileResultRelPath2)
    ]
}

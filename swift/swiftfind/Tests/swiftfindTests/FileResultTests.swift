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
        let path = "\(NSHomeDirectory())/src/xfind/swift/swiftfind/Sources/swiftfind/FileResult.swift"
        let fileResult = FileResult(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, fileResult.description)
    }

    func testFileResultTildePath() {
        let path = "~/src/xfind/swift/swiftfind/Sources/swiftfind/FileResult.swift"
        let fileResult = FileResult(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, fileResult.description)
    }

    func testFileResultRelPath1() {
        let path = "./FileResult.swift"
        let fileResult = FileResult(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, fileResult.description)
    }

    func testFileResultRelPath2() {
        let path = "../FileResult.swift"
        let fileResult = FileResult(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, fileResult.description)
    }

    static var allTests = [
        ("testFileResultAbsPath", testFileResultAbsPath),
        ("testFileResultTildePath", testFileResultTildePath),
        ("testFileResultRelPath1", testFileResultRelPath1),
        ("testFileResultRelPath2", testFileResultRelPath2),
    ]
}

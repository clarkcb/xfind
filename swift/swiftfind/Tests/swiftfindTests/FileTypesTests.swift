//
//  FileTypesTests.swift
//  swiftfindTests
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import XCTest

import swiftfind

class FileTypesTests: XCTestCase {
    let fileTypes = FileTypes()

//    override func setUp() {
//        super.setUp()
//    }
//
//    override func tearDown() {
//        super.tearDown()
//    }

    func testArchiveFiles() {
        let exts = ["7z", "arj", "bz2", "cpio", "ear", "gz", "hqx", "jar",
                    "pax", "rar", "sit", "sitx", "tar", "tgz", "war", "zip", "zipx",
                    "Z"]
        for ext in exts {
            let fileName = "archive.\(ext)"
            XCTAssert(fileTypes.isArchiveFile(fileName), "isArchiveFile(\(fileName)) == true")
            XCTAssert(!fileTypes.isBinaryFile(fileName), "isBinaryFile(\(fileName)) == false")
            XCTAssert(!fileTypes.isTextFile(fileName), "isTextFile(\(fileName)) == false")
            XCTAssert(fileTypes.getFileType(fileName) == FileType.archive,
                      "\(fileName) is archive file")
        }
    }

    func testAudioFile() {
        let fileName = "music.mp3"
        XCTAssert(fileTypes.isAudioFile(fileName), "isAudioFile(\(fileName)) == true")
        XCTAssert(fileTypes.getFileType(fileName) == FileType.audio, "\(fileName) is audio file")
    }

    func testBinaryFiles() {
        let exts = ["a", "beam", "chm", "class", "com", "dat", "dbmdl", "dcr", "dir", "dll",
                    "dms", "doc", "dot", "dxr", "dylib", "epub", "exe", "fm", "hi", "hlp",
                    "lib", "lnk", "mdb", "mo", "mobi", "mpp", "nib", "o", "obj", "odm", "odt",
                    "ott", "pages", "pdb", "ppt", "pub", "pyc", "pyo", "qxd", "rpt", "so",
                    "swf", "sys", "vsd", "wpd", "wps", "wpt", "wri", "xls", "xlt"]
        for ext in exts {
            let fileName = "binfile.\(ext)"
            XCTAssert(!fileTypes.isArchiveFile(fileName), "isArchiveFile(\(fileName)) == false")
            XCTAssert(fileTypes.isBinaryFile(fileName), "isBinaryFile(\(fileName)) == true")
            XCTAssert(!fileTypes.isTextFile(fileName), "isTextFile(\(fileName)) == false")
            XCTAssert(fileTypes.getFileType(fileName) == FileType.binary,
                      "\(fileName) is binary file")
        }
    }

    func testCodeFile() {
        let fileName = "code.swift"
        XCTAssert(fileTypes.isCodeFile(fileName), "isCodeFile(\(fileName)) == true")
        XCTAssert(fileTypes.getFileType(fileName) == FileType.code, "\(fileName) is code file")
    }

    func testFontFile() {
        let fileName = "font.ttf"
        XCTAssert(fileTypes.isFontFile(fileName), "isFontFile(\(fileName)) == true")
        XCTAssert(fileTypes.getFileType(fileName) == FileType.font, "\(fileName) is font file")
    }

    func testImageFile() {
        let fileName = "image.png"
        XCTAssert(fileTypes.isImageFile(fileName), "isImageFile(\(fileName)) == true")
        XCTAssert(fileTypes.getFileType(fileName) == FileType.image, "\(fileName) is image file")
    }

    func testTextFile() {
        let fileName = "text.txt"
        XCTAssert(fileTypes.isTextFile(fileName), "isTextFile(\(fileName)) == true")
        XCTAssert(fileTypes.getFileType(fileName) == FileType.text, "\(fileName) is text file")
    }

    func testVideoFile() {
        let fileName = "movie.mp4"
        XCTAssert(fileTypes.isVideoFile(fileName), "isVideoFile(\(fileName)) == true")
        XCTAssert(fileTypes.getFileType(fileName) == FileType.video, "\(fileName) is video file")
    }

    func testXmlFile() {
        let fileName = "markup.xml"
        XCTAssert(fileTypes.isXmlFile(fileName), "isXmlFile(\(fileName)) == true")
        XCTAssert(fileTypes.getFileType(fileName) == FileType.xml, "\(fileName) is xml file")
    }

    func testUnknownFile() {
        let fileName = "unknown.ZZZ"
        XCTAssert(fileTypes.getFileType(fileName) == FileType.unknown, "\(fileName) is unknown file")
    }

    static let allTests = [
        ("testArchiveFiles", testArchiveFiles),
        ("testBinaryFiles", testBinaryFiles),
        ("testCodeFile", testCodeFile),
        ("testTextFile", testTextFile),
        ("testXmlFile", testXmlFile),
        ("testUnknownFile", testUnknownFile),
    ]
}

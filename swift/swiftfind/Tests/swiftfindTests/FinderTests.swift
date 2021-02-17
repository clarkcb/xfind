import Cocoa
import XCTest

import swiftfind

class FinderTests: XCTestCase {
    let fileTypes = FileTypes()

    func getSettings() -> FindSettings {
        let settings = FindSettings()
        settings.startPath = "."
        settings.addFindPattern("Finder")
        return settings
    }

    override func setUp() {
        super.setUp()
    }

    override func tearDown() {
        super.tearDown()
    }

    /* ==========================================================================
     * isFindDir tests
     ========================================================================= */
    func testIsFindDir_SingleDot_True() {
        var error: NSError?
        let settings = getSettings()
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindDir("."))
    }

    func testIsFindDir_DoubleDot_True() {
        var error: NSError?
        let settings = getSettings()
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindDir(".."))
    }

    func testIsFindDir_IsHidden_False() {
        var error: NSError?
        let settings = getSettings()
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isFindDir(".git"))
    }

    func testIsFindDir_IsHiddenIncludeHidden_True() {
        var error: NSError?
        let settings = getSettings()
        settings.excludeHidden = false
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindDir(".git"))
    }

    func testIsFindDir_NoPatterns_True() {
        var error: NSError?
        let settings = getSettings()
        settings.excludeHidden = false
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindDir("/Users"))
    }

    func testIsFindDir_MatchesInPattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInDirPattern("Find")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindDir("CsFind"))
    }

    func testIsFindDir_DoesNotMatchInPattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInDirPattern("FindFiles")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isFindDir("CsFind"))
    }

    func testIsFindDir_MatchesOutPattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutDirPattern("Find")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isFindDir("CsFind"))
    }

    func testIsFindDir_DoesNotMatchOutPattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutDirPattern("FindFiles")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindDir("CsFind"))
    }

    /* ==========================================================================
     * isFindFile tests
     ========================================================================= */
    func testIsFindFile_NoExtensionsNoPatterns_True() {
        var error: NSError?
        let settings = getSettings()
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_MatchesInExtension_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInExtension("cs")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_DoesNotMatchInExtension_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInExtension("java")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_MatchesOutExtension_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutExtension("cs")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_DoesNotMatchOutExtension_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutExtension("java")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_MatchesInFilePattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInFilePattern("Find")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindFile("Finder.cs"))
    }

    func testIsFindFile_DoesNotMatchInFilePattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInFilePattern("Find")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_MatchesOutFilePattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutFilePattern("Find")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isFindFile("Finder.cs"))
    }

    func testIsFindFile_DoesNotMatchOutFilePattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutFilePattern("Find")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isFindFile("FileUtil.cs"))
    }

    /* ==========================================================================
     * isArchiveFindFile tests
     ========================================================================= */
    func testIsArchiveFindFile_NoExtensionsNoPatterns_True() {
        var error: NSError?
        let settings = getSettings()
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_MatchesInExtension_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInArchiveExtension("zip")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_DoesNotMatchInExtension_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInArchiveExtension("gz")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_MatchesOutExtension_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutArchiveExtension("zip")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_DoesNotMatchOutExtension_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutArchiveExtension("gz")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_MatchesInArchiveFilePattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInArchiveFilePattern("arch")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_DoesNotMatchInArchiveFilePattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInArchiveFilePattern("archives")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_MatchesOutArchiveFilePattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutArchiveFilePattern("arch")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_DoesNotMatchOutArchiveFilePattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutArchiveFilePattern("archives")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    /* ==========================================================================
     * filterFile tests
     ========================================================================= */
    func testFilterFile_IsHidden_False() {
        var error: NSError?
        let settings = getSettings()
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.filterFile(".gitignore"))
    }

    func testFilterFile_IsHiddenIncludeHidden_True() {
        var error: NSError?
        let settings = getSettings()
        settings.excludeHidden = false
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.filterFile(".hidden.txt"))
    }

    func testFilterFile_ArchiveNoFindArchives_False() {
        var error: NSError?
        let settings = getSettings()
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.filterFile("archive.zip"))
    }

    func testFilterFile_ArchiveFindArchives_True() {
        var error: NSError?
        let settings = getSettings()
        settings.findArchives = true
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.filterFile("archive.zip"))
    }

    func testFilterFile_IsArchiveFindFile_True() {
        var error: NSError?
        let settings = getSettings()
        settings.findArchives = true
        settings.addInArchiveExtension("zip")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.filterFile("archive.zip"))
    }

    func testFilterFile_NotIsArchiveFindFile_False() {
        var error: NSError?
        let settings = getSettings()
        settings.findArchives = true
        settings.addOutArchiveExtension("zip")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.filterFile("archive.zip"))
    }

    func testFilterFile_ArchiveFileArchivesOnly_True() {
        var error: NSError?
        let settings = getSettings()
        settings.archivesOnly = true
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.filterFile("archive.zip"))
    }

    func testFilterFile_NoExtensionsNoPatterns_True() {
        var error: NSError?
        let settings = getSettings()
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.filterFile("FileUtil.cs"))
    }

    func testFilterFile_IsFindFile_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInExtension("cs")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertTrue(finder.filterFile("FileUtil.cs"))
    }

    func testFilterFile_NotIsFindFile_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutExtension("cs")
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.filterFile("FileUtil.cs"))
    }

    func testFilterFile_NonArchiveFileArchivesOnly_False() {
        var error: NSError?
        let settings = getSettings()
        settings.archivesOnly = true
        let finder = Finder(settings: settings, error: &error)
        XCTAssertFalse(finder.filterFile("FileUtil.cs"))
    }

    /* ==========================================================================
     * findLineReader tests
     ========================================================================= */
    func testFindLineReader() {
        var error: NSError?
        let settings = getSettings()
        let finder = Finder(settings: settings, error: &error)
        let testFilePath = FileUtil.joinPath(Config.sharedPath, childPath: "testFiles/testFile2.txt")

        if let reader = StreamReader(path: testFilePath, encoding: .utf8) {
            let results = finder.findLineReader(reader)

            XCTAssert(results.count == 2)

            XCTAssertEqual(29, results[0].lineNum)
            XCTAssertEqual(3, results[0].matchStartIndex)
            XCTAssertEqual(11, results[0].matchEndIndex)

            XCTAssertEqual(35, results[1].lineNum)
            XCTAssertEqual(24, results[1].matchStartIndex)
            XCTAssertEqual(32, results[1].matchEndIndex)

            reader.close()

        } else {
            XCTAssertTrue(false)
        }
    }

    /* ==========================================================================
     * findMultiLineString tests
     ========================================================================= */
    func testFindMultiLineString() {
        var error: NSError?
        let settings = getSettings()
        settings.multiLineFind = true
        let finder = Finder(settings: settings, error: &error)
        let testFilePath = FileUtil.joinPath(Config.sharedPath, childPath: "testFiles/testFile2.txt")
        let testFileContents = try? String(contentsOfFile: testFilePath, encoding: .utf8)
        if testFileContents != nil {
            let results = finder.findMultiLineString(testFileContents!)

            XCTAssert(results.count == 2)

            XCTAssertEqual(29, results[0].lineNum)
            XCTAssertEqual(3, results[0].matchStartIndex)
            XCTAssertEqual(11, results[0].matchEndIndex)

            XCTAssertEqual(35, results[1].lineNum)
            XCTAssertEqual(24, results[1].matchStartIndex)
            XCTAssertEqual(32, results[1].matchEndIndex)

        } else {
            XCTAssertTrue(false)
        }
    }
}

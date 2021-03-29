import XCTest

import swiftfind

class FinderTests: XCTestCase {
    let fileTypes = FileTypes()

    func getSettings() -> FindSettings {
        let settings = FindSettings()
        settings.addPath(".")
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
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindDir("."))
    }

    func testIsFindDir_DoubleDot_True() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindDir(".."))
    }

    func testIsFindDir_IsHidden_False() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isFindDir(".git"))
    }

    func testIsFindDir_IsHiddenIncludeHidden_True() {
        let settings = getSettings()
        settings.excludeHidden = false
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindDir(".git"))
    }

    func testIsFindDir_NoPatterns_True() {
        let settings = getSettings()
        settings.excludeHidden = false
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindDir("/Users"))
    }

    func testIsFindDir_MatchesInPattern_True() {
        let settings = getSettings()
        settings.addInDirPattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindDir("CsFind"))
    }

    func testIsFindDir_DoesNotMatchInPattern_False() {
        let settings = getSettings()
        settings.addInDirPattern("FindFiles")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isFindDir("CsFind"))
    }

    func testIsFindDir_MatchesOutPattern_False() {
        let settings = getSettings()
        settings.addOutDirPattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isFindDir("CsFind"))
    }

    func testIsFindDir_DoesNotMatchOutPattern_True() {
        let settings = getSettings()
        settings.addOutDirPattern("FindFiles")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindDir("CsFind"))
    }

    /* ==========================================================================
     * isFindFile tests
     ========================================================================= */
    func testIsFindFile_NoExtensionsNoPatterns_True() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_MatchesInExtension_True() {
        let settings = getSettings()
        settings.addInExtension("cs")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_DoesNotMatchInExtension_False() {
        let settings = getSettings()
        settings.addInExtension("java")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_MatchesOutExtension_False() {
        let settings = getSettings()
        settings.addOutExtension("cs")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_DoesNotMatchOutExtension_True() {
        let settings = getSettings()
        settings.addOutExtension("java")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_MatchesInFilePattern_True() {
        let settings = getSettings()
        settings.addInFilePattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindFile("Finder.cs"))
    }

    func testIsFindFile_DoesNotMatchInFilePattern_False() {
        let settings = getSettings()
        settings.addInFilePattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isFindFile("FileUtil.cs"))
    }

    func testIsFindFile_MatchesOutFilePattern_False() {
        let settings = getSettings()
        settings.addOutFilePattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isFindFile("Finder.cs"))
    }

    func testIsFindFile_DoesNotMatchOutFilePattern_True() {
        let settings = getSettings()
        settings.addOutFilePattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isFindFile("FileUtil.cs"))
    }

    /* ==========================================================================
     * isArchiveFindFile tests
     ========================================================================= */
    func testIsArchiveFindFile_NoExtensionsNoPatterns_True() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_MatchesInExtension_True() {
        let settings = getSettings()
        settings.addInArchiveExtension("zip")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_DoesNotMatchInExtension_False() {
        let settings = getSettings()
        settings.addInArchiveExtension("gz")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_MatchesOutExtension_False() {
        let settings = getSettings()
        settings.addOutArchiveExtension("zip")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_DoesNotMatchOutExtension_True() {
        let settings = getSettings()
        settings.addOutArchiveExtension("gz")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_MatchesInArchiveFilePattern_True() {
        let settings = getSettings()
        settings.addInArchiveFilePattern("arch")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_DoesNotMatchInArchiveFilePattern_False() {
        let settings = getSettings()
        settings.addInArchiveFilePattern("archives")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_MatchesOutArchiveFilePattern_False() {
        let settings = getSettings()
        settings.addOutArchiveFilePattern("arch")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isArchiveFindFile("archive.zip"))
    }

    func testIsArchiveFindFile_DoesNotMatchOutArchiveFilePattern_True() {
        let settings = getSettings()
        settings.addOutArchiveFilePattern("archives")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isArchiveFindFile("archive.zip"))
    }

    /* ==========================================================================
     * filterToFindFile tests
     ========================================================================= */
    func testFilterToFindFile_IsHidden_False() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile(".gitignore") == nil)
    }

    func testFilterToFindFile_IsHiddenIncludeHidden_True() {
        let settings = getSettings()
        settings.excludeHidden = false
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile(".hidden.txt") != nil)
    }

    func testFilterToFindFile_ArchiveNoIncludeArchives_False() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile("archive.zip") == nil)
    }

    func testFilterToFindFile_ArchiveIncludeArchives_True() {
        let settings = getSettings()
        settings.includeArchives = true
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile("archive.zip") != nil)
    }

    func testFilterToFindFile_IsArchiveFindFile_True() {
        let settings = getSettings()
        settings.includeArchives = true
        settings.addInArchiveExtension("zip")
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile("archive.zip") != nil)
    }

    func testFilterToFindFile_NotIsArchiveFindFile_False() {
        let settings = getSettings()
        settings.includeArchives = true
        settings.addOutArchiveExtension("zip")
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile("archive.zip") == nil)
    }

    func testFilterToFindFile_ArchiveFileArchivesOnly_True() {
        let settings = getSettings()
        settings.archivesOnly = true
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile("archive.zip") != nil)
    }

    func testFilterToFindFile_NoExtensionsNoPatterns_True() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile("FileUtil.cs") != nil)
    }

    func testFilterToFindFile_IsFindFile_True() {
        let settings = getSettings()
        settings.addInExtension("cs")
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile("FileUtil.cs") != nil)
    }

    func testFilterToFindFile_NotIsFindFile_False() {
        let settings = getSettings()
        settings.addOutExtension("cs")
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile("FileUtil.cs") == nil)
    }

    func testFilterToFindFile_NonArchiveFileArchivesOnly_False() {
        let settings = getSettings()
        settings.archivesOnly = true
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFindFile("FileUtil.cs") == nil)
    }

    static var allTests = [
        ("testIsFindDir_SingleDot_True", testIsFindDir_SingleDot_True),
        ("testIsFindDir_DoubleDot_True", testIsFindDir_DoubleDot_True),
        ("testIsFindDir_IsHidden_False", testIsFindDir_IsHidden_False),
        ("testIsFindDir_IsHiddenIncludeHidden_True", testIsFindDir_IsHiddenIncludeHidden_True),
        ("testIsFindDir_NoPatterns_True", testIsFindDir_NoPatterns_True),
        ("testIsFindDir_MatchesInPattern_True", testIsFindDir_MatchesInPattern_True),
        ("testIsFindDir_DoesNotMatchInPattern_False", testIsFindDir_DoesNotMatchInPattern_False),
        ("testIsFindDir_MatchesOutPattern_False", testIsFindDir_MatchesOutPattern_False),
        ("testIsFindDir_DoesNotMatchOutPattern_True", testIsFindDir_DoesNotMatchOutPattern_True),
        ("testIsFindFile_NoExtensionsNoPatterns_True", testIsFindFile_NoExtensionsNoPatterns_True),
        ("testIsFindFile_MatchesInExtension_True", testIsFindFile_MatchesInExtension_True),
        ("testIsFindFile_DoesNotMatchInExtension_False", testIsFindFile_DoesNotMatchInExtension_False),
        ("testIsFindFile_MatchesOutExtension_False", testIsFindFile_MatchesOutExtension_False),
        ("testIsFindFile_DoesNotMatchOutExtension_True", testIsFindFile_DoesNotMatchOutExtension_True),
        ("testIsFindFile_MatchesInFilePattern_True", testIsFindFile_MatchesInFilePattern_True),
        ("testIsFindFile_DoesNotMatchInFilePattern_False", testIsFindFile_DoesNotMatchInFilePattern_False),
        ("testIsFindFile_MatchesOutFilePattern_False", testIsFindFile_MatchesOutFilePattern_False),
        ("testIsFindFile_DoesNotMatchOutFilePattern_True", testIsFindFile_DoesNotMatchOutFilePattern_True),
        ("testIsArchiveFindFile_NoExtensionsNoPatterns_True", testIsArchiveFindFile_NoExtensionsNoPatterns_True),
        ("testIsArchiveFindFile_MatchesInExtension_True", testIsArchiveFindFile_MatchesInExtension_True),
        ("testIsArchiveFindFile_DoesNotMatchInExtension_False", testIsArchiveFindFile_DoesNotMatchInExtension_False),
        ("testIsArchiveFindFile_MatchesOutExtension_False", testIsArchiveFindFile_MatchesOutExtension_False),
        ("testIsArchiveFindFile_DoesNotMatchOutExtension_True", testIsArchiveFindFile_DoesNotMatchOutExtension_True),
        ("testIsArchiveFindFile_MatchesInArchiveFilePattern_True", testIsArchiveFindFile_MatchesInArchiveFilePattern_True),
        ("testIsArchiveFindFile_DoesNotMatchInArchiveFilePattern_False", testIsArchiveFindFile_DoesNotMatchInArchiveFilePattern_False),
        ("testIsArchiveFindFile_MatchesOutArchiveFilePattern_False", testIsArchiveFindFile_MatchesOutArchiveFilePattern_False),
        ("testIsArchiveFindFile_DoesNotMatchOutArchiveFilePattern_True", testIsArchiveFindFile_DoesNotMatchOutArchiveFilePattern_True),
        ("testFilterToFindFile_IsHidden_False", testFilterToFindFile_IsHidden_False),
        ("testFilterToFindFile_IsHiddenIncludeHidden_True", testFilterToFindFile_IsHiddenIncludeHidden_True),
        ("testFilterToFindFile_ArchiveNoIncludeArchives_False", testFilterToFindFile_ArchiveNoIncludeArchives_False),
        ("testFilterToFindFile_ArchiveIncludeArchives_True", testFilterToFindFile_ArchiveIncludeArchives_True),
        ("testFilterToFindFile_IsArchiveFindFile_True", testFilterToFindFile_IsArchiveFindFile_True),
        ("testFilterToFindFile_NotIsArchiveFindFile_False", testFilterToFindFile_NotIsArchiveFindFile_False),
        ("testFilterToFindFile_ArchiveFileArchivesOnly_True", testFilterToFindFile_ArchiveFileArchivesOnly_True),
        ("testFilterToFindFile_NoExtensionsNoPatterns_True", testFilterToFindFile_NoExtensionsNoPatterns_True),
        ("testFilterToFindFile_IsFindFile_True", testFilterToFindFile_IsFindFile_True),
        ("testFilterToFindFile_NotIsFindFile_False", testFilterToFindFile_NotIsFindFile_False),
        ("testFilterToFindFile_NonArchiveFileArchivesOnly_False", testFilterToFindFile_NonArchiveFileArchivesOnly_False),
    ]
}

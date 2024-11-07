import XCTest

import swiftfind

class FinderTests: XCTestCase {
    let fileTypes = FileTypes()

    func getSettings() -> FindSettings {
        let settings = FindSettings()
        settings.addPath(".")
        return settings
    }

    func getBinPath() -> String {
        let config = FindConfig()
        return FileUtil.joinPath(config.xfindPath, childPath: "bin")
    }

//    override func setUp() {
//        super.setUp()
//    }
//
//    override func tearDown() {
//        super.tearDown()
//    }

    /* ==========================================================================
     * isMatchingDir tests
     ========================================================================= */
    func testIsMatchingDir_SingleDot_True() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingDir("."))
    }

    func testIsMatchingDir_DoubleDot_True() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingDir(".."))
    }

    func testIsMatchingDir_IsHidden_False() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingDir(".git"))
    }

    func testIsMatchingDir_IsHiddenIncludeHidden_True() {
        let settings = getSettings()
        settings.includeHidden = true
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingDir(".git"))
    }

    func testIsMatchingDir_NoPatterns_True() {
        let settings = getSettings()
        settings.includeHidden = false
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingDir("/Users"))
    }

    func testIsMatchingDir_MatchesInPattern_True() {
        let settings = getSettings()
        settings.addInDirPattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingDir("CsFind"))
    }

    func testIsMatchingDir_DoesNotMatchInPattern_False() {
        let settings = getSettings()
        settings.addInDirPattern("FindFiles")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingDir("CsFind"))
    }

    func testIsMatchingDir_MatchesOutPattern_False() {
        let settings = getSettings()
        settings.addOutDirPattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingDir("CsFind"))
    }

    func testIsMatchingDir_DoesNotMatchOutPattern_True() {
        let settings = getSettings()
        settings.addOutDirPattern("FindFiles")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingDir("CsFind"))
    }

    /* ==========================================================================
     * isMatchingFile tests
     ========================================================================= */
    func testIsMatchingFile_NoExtensionsNoPatterns_True() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingFile("FileUtil.cs"))
    }

    func testIsMatchingFile_MatchesInExtension_True() {
        let settings = getSettings()
        settings.addInExtension("cs")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingFile("FileUtil.cs"))
    }

    func testIsMatchingFile_DoesNotMatchInExtension_False() {
        let settings = getSettings()
        settings.addInExtension("java")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingFile("FileUtil.cs"))
    }

    func testIsMatchingFile_MatchesOutExtension_False() {
        let settings = getSettings()
        settings.addOutExtension("cs")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingFile("FileUtil.cs"))
    }

    func testIsMatchingFile_DoesNotMatchOutExtension_True() {
        let settings = getSettings()
        settings.addOutExtension("java")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingFile("FileUtil.cs"))
    }

    func testIsMatchingFile_MatchesInFilePattern_True() {
        let settings = getSettings()
        settings.addInFilePattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingFile("Finder.cs"))
    }

    func testIsMatchingFile_DoesNotMatchInFilePattern_False() {
        let settings = getSettings()
        settings.addInFilePattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingFile("FileUtil.cs"))
    }

    func testIsMatchingFile_MatchesOutFilePattern_False() {
        let settings = getSettings()
        settings.addOutFilePattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingFile("Finder.cs"))
    }

    func testIsMatchingFile_DoesNotMatchOutFilePattern_True() {
        let settings = getSettings()
        settings.addOutFilePattern("Find")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingFile("FileUtil.cs"))
    }

    /* ==========================================================================
     * isMatchingArchiveFile tests
     ========================================================================= */
    func testIsMatchingArchiveFile_NoExtensionsNoPatterns_True() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingArchiveFile("archive.zip"))
    }

    func testIsMatchingArchiveFile_MatchesInExtension_True() {
        let settings = getSettings()
        settings.addInArchiveExtension("zip")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingArchiveFile("archive.zip"))
    }

    func testIsMatchingArchiveFile_DoesNotMatchInExtension_False() {
        let settings = getSettings()
        settings.addInArchiveExtension("gz")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingArchiveFile("archive.zip"))
    }

    func testIsMatchingArchiveFile_MatchesOutExtension_False() {
        let settings = getSettings()
        settings.addOutArchiveExtension("zip")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingArchiveFile("archive.zip"))
    }

    func testIsMatchingArchiveFile_DoesNotMatchOutExtension_True() {
        let settings = getSettings()
        settings.addOutArchiveExtension("gz")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingArchiveFile("archive.zip"))
    }

    func testIsMatchingArchiveFile_MatchesInArchiveFilePattern_True() {
        let settings = getSettings()
        settings.addInArchiveFilePattern("arch")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingArchiveFile("archive.zip"))
    }

    func testIsMatchingArchiveFile_DoesNotMatchInArchiveFilePattern_False() {
        let settings = getSettings()
        settings.addInArchiveFilePattern("archives")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingArchiveFile("archive.zip"))
    }

    func testIsMatchingArchiveFile_MatchesOutArchiveFilePattern_False() {
        let settings = getSettings()
        settings.addOutArchiveFilePattern("arch")
        let finder = try! Finder(settings: settings)
        XCTAssertFalse(finder.isMatchingArchiveFile("archive.zip"))
    }

    func testIsMatchingArchiveFile_DoesNotMatchOutArchiveFilePattern_True() {
        let settings = getSettings()
        settings.addOutArchiveFilePattern("archives")
        let finder = try! Finder(settings: settings)
        XCTAssertTrue(finder.isMatchingArchiveFile("archive.zip"))
    }

    /* ==========================================================================
     * filterToFileResult tests
     ========================================================================= */
    func testFilterToFileResult_IsHidden_False() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult(".gitignore") == nil)
    }

    func testFilterToFileResult_IsHiddenIncludeHidden_True() {
        let settings = getSettings()
        settings.includeHidden = true
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult(".hidden.txt") != nil)
    }

    func testFilterToFileResult_ArchiveNoIncludeArchives_False() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult("archive.zip") == nil)
    }

    func testFilterToFileResult_ArchiveIncludeArchives_True() {
        let settings = getSettings()
        settings.includeArchives = true
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult("archive.zip") != nil)
    }

    func testFilterToFileResult_IsArchiveFindFile_True() {
        let settings = getSettings()
        settings.includeArchives = true
        settings.addInArchiveExtension("zip")
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult("archive.zip") != nil)
    }

    func testFilterToFileResult_NotIsArchiveFindFile_False() {
        let settings = getSettings()
        settings.includeArchives = true
        settings.addOutArchiveExtension("zip")
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult("archive.zip") == nil)
    }

    func testFilterToFileResult_ArchiveFileArchivesOnly_True() {
        let settings = getSettings()
        settings.archivesOnly = true
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult("archive.zip") != nil)
    }

    func testFilterToFileResult_NoExtensionsNoPatterns_True() {
        let settings = getSettings()
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult("FileUtil.cs") != nil)
    }

    func testFilterToFileResult_IsFindFile_True() {
        let settings = getSettings()
        settings.addInExtension("cs")
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult("FileUtil.cs") != nil)
    }

    func testFilterToFileResult_NotIsFindFile_False() {
        let settings = getSettings()
        settings.addOutExtension("cs")
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult("FileUtil.cs") == nil)
    }

    func testFilterToFileResult_NonArchiveFileArchivesOnly_False() {
        let settings = getSettings()
        settings.archivesOnly = true
        let finder = try! Finder(settings: settings)
        XCTAssert(finder.filterToFileResult("FileUtil.cs") == nil)
    }

    /* ==========================================================================
     * followSymlinks tests
     ========================================================================= */
    func testFollowSymlinks_DefaultSettings_Excluded() {
        let settings = FindSettings()
        settings.addPath(getBinPath())
        let finder = try! Finder(settings: settings)
        let fileResults = try! finder.find()
        XCTAssert(fileResults.count < 3)
    }

    func testFollowSymlinks_FollowSymlinks_Included() {
        let settings = FindSettings()
        settings.addPath(getBinPath())
        settings.followSymlinks = true
        let finder = try! Finder(settings: settings)
        let fileResults = try! finder.find()
        XCTAssert(fileResults.count == 0 || fileResults.count > 2)
    }

    func testFollowSymlinks_NoFollowSymlinks_Excluded() {
        let settings = FindSettings()
        settings.addPath(getBinPath())
        settings.followSymlinks = false
        let finder = try! Finder(settings: settings)
        let fileResults = try! finder.find()
        XCTAssert(fileResults.count < 3)
    }

    static var allTests = [
        ("testIsMatchingDir_SingleDot_True", testIsMatchingDir_SingleDot_True),
        ("testIsMatchingDir_DoubleDot_True", testIsMatchingDir_DoubleDot_True),
        ("testIsMatchingDir_IsHidden_False", testIsMatchingDir_IsHidden_False),
        ("testIsMatchingDir_IsHiddenIncludeHidden_True", testIsMatchingDir_IsHiddenIncludeHidden_True),
        ("testIsMatchingDir_NoPatterns_True", testIsMatchingDir_NoPatterns_True),
        ("testIsMatchingDir_MatchesInPattern_True", testIsMatchingDir_MatchesInPattern_True),
        ("testIsMatchingDir_DoesNotMatchInPattern_False", testIsMatchingDir_DoesNotMatchInPattern_False),
        ("testIsMatchingDir_MatchesOutPattern_False", testIsMatchingDir_MatchesOutPattern_False),
        ("testIsMatchingDir_DoesNotMatchOutPattern_True", testIsMatchingDir_DoesNotMatchOutPattern_True),
        ("testIsMatchingFile_NoExtensionsNoPatterns_True", testIsMatchingFile_NoExtensionsNoPatterns_True),
        ("testIsMatchingFile_MatchesInExtension_True", testIsMatchingFile_MatchesInExtension_True),
        ("testIsMatchingFile_DoesNotMatchInExtension_False", testIsMatchingFile_DoesNotMatchInExtension_False),
        ("testIsMatchingFile_MatchesOutExtension_False", testIsMatchingFile_MatchesOutExtension_False),
        ("testIsMatchingFile_DoesNotMatchOutExtension_True", testIsMatchingFile_DoesNotMatchOutExtension_True),
        ("testIsMatchingFile_MatchesInFilePattern_True", testIsMatchingFile_MatchesInFilePattern_True),
        ("testIsMatchingFile_DoesNotMatchInFilePattern_False", testIsMatchingFile_DoesNotMatchInFilePattern_False),
        ("testIsMatchingFile_MatchesOutFilePattern_False", testIsMatchingFile_MatchesOutFilePattern_False),
        ("testIsMatchingFile_DoesNotMatchOutFilePattern_True", testIsMatchingFile_DoesNotMatchOutFilePattern_True),
        ("testIsMatchingArchiveFile_NoExtensionsNoPatterns_True",
         testIsMatchingArchiveFile_NoExtensionsNoPatterns_True),
        ("testIsMatchingArchiveFile_MatchesInExtension_True", testIsMatchingArchiveFile_MatchesInExtension_True),
        ("testIsMatchingArchiveFile_DoesNotMatchInExtension_False",
         testIsMatchingArchiveFile_DoesNotMatchInExtension_False),
        ("testIsMatchingArchiveFile_MatchesOutExtension_False", testIsMatchingArchiveFile_MatchesOutExtension_False),
        ("testIsMatchingArchiveFile_DoesNotMatchOutExtension_True",
         testIsMatchingArchiveFile_DoesNotMatchOutExtension_True),
        ("testIsMatchingArchiveFile_MatchesInArchiveFilePattern_True",
         testIsMatchingArchiveFile_MatchesInArchiveFilePattern_True),
        ("testIsMatchingArchiveFile_DoesNotMatchInArchiveFilePattern_False",
         testIsMatchingArchiveFile_DoesNotMatchInArchiveFilePattern_False),
        ("testIsMatchingArchiveFile_MatchesOutArchiveFilePattern_False",
         testIsMatchingArchiveFile_MatchesOutArchiveFilePattern_False),
        ("testIsMatchingArchiveFile_DoesNotMatchOutArchiveFilePattern_True",
         testIsMatchingArchiveFile_DoesNotMatchOutArchiveFilePattern_True),
        ("testFilterToFileResult_IsHidden_False", testFilterToFileResult_IsHidden_False),
        ("testFilterToFileResult_IsHiddenIncludeHidden_True", testFilterToFileResult_IsHiddenIncludeHidden_True),
        ("testFilterToFileResult_ArchiveNoIncludeArchives_False",
         testFilterToFileResult_ArchiveNoIncludeArchives_False),
        ("testFilterToFileResult_ArchiveIncludeArchives_True", testFilterToFileResult_ArchiveIncludeArchives_True),
        ("testFilterToFileResult_IsArchiveFindFile_True", testFilterToFileResult_IsArchiveFindFile_True),
        ("testFilterToFileResult_NotIsArchiveFindFile_False", testFilterToFileResult_NotIsArchiveFindFile_False),
        ("testFilterToFileResult_ArchiveFileArchivesOnly_True", testFilterToFileResult_ArchiveFileArchivesOnly_True),
        ("testFilterToFileResult_NoExtensionsNoPatterns_True", testFilterToFileResult_NoExtensionsNoPatterns_True),
        ("testFilterToFileResult_IsFindFile_True", testFilterToFileResult_IsFindFile_True),
        ("testFilterToFileResult_NotIsFindFile_False", testFilterToFileResult_NotIsFindFile_False),
        ("testFilterToFileResult_NonArchiveFileArchivesOnly_False",
         testFilterToFileResult_NonArchiveFileArchivesOnly_False),
        // followSymlinks tests
        ("testFollowSymlinks_DefaultSettings_Excluded", testFollowSymlinks_DefaultSettings_Excluded),
        ("testFollowSymlinks_FollowSymlinks_Included", testFollowSymlinks_FollowSymlinks_Included),
        ("testFollowSymlinks_NoFollowSymlinks_Excluded", testFollowSymlinks_NoFollowSymlinks_Excluded)
    ]
}

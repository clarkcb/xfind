package ktfind

import java.nio.file.Paths
import kotlin.test.*

/**
 * @author cary on 7/30/16.
 */
class FinderTest {
    private fun getConfig(): FindConfig {
        return FindConfig()
    }

    private fun getSettings(): FindSettings {
        return getDefaultSettings().copy(paths = setOf(Paths.get(".")))
    }

    private fun getBinPath(): String {
        var xfindPath = System.getenv("XFIND_PATH")
        if (xfindPath == null) {
            xfindPath = System.getenv("HOME") + "src/xfind"
        }
        return "$xfindPath/bin"
    }

//    private val testFilePath = "/testFile2.txt"

    /***************************************************************************
     * filterToFileResult tests
     **************************************************************************/
    @Test
    fun testFilterToFileResult_IsHidden_Null() {
        val config = getConfig()
        val settings = getSettings()
        val finder = Finder(config, settings)
        val file = Paths.get(".gitignore")
        assertNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_IsHiddenIncludeHidden_NotNull() {
        val config = getConfig()
        val settings = getSettings().copy(includeHidden = true)
        val finder = Finder(config, settings)
        val file = Paths.get(".gitignore")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_ArchiveNoIncludeArchives_Null() {
        val config = getConfig()
        val settings = getSettings()
        val finder = Finder(config, settings)
        val file = Paths.get("archive.zip")
        val fileResult = finder.filterToFileResult(file)
        assertNull(fileResult)
    }

    @Test
    fun testFilterToFileResult_ArchiveIncludeArchives_NotNull() {
        val config = getConfig()
        val settings = getSettings().copy(includeArchives = true)
        val finder = Finder(config, settings)
        val file = Paths.get("archive.zip")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_IsMatchingArchiveFile_NotNull() {
        val config = getConfig()
        val settings = getSettings().copy(includeArchives = true, inArchiveExtensions = setOf("zip"))
        val finder = Finder(config, settings)
        val file = Paths.get("archive.zip")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_NotIsMatchingArchiveFile_Null() {
        val config = getConfig()
        val settings = getSettings().copy(outExtensions = setOf("zip"))
        val finder = Finder(config, settings)
        val file = Paths.get("archive.zip")
        assertNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_ArchiveFileArchivesOnly_NotNull() {
        val config = getConfig()
        val settings = getSettings().copy(archivesOnly = true)
        val finder = Finder(config, settings)
        val file = Paths.get("archive.zip")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_NoExtensionsNoPatterns_NotNull() {
        val config = getConfig()
        val settings = getSettings()
        val finder = Finder(config, settings)
        val file = Paths.get("FileUtil.cs")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_IsMatchingFile_NotNull() {
        val config = getConfig()
        val settings = getSettings().copy(inExtensions = setOf("cs"))
        val finder = Finder(config, settings)
        val file = Paths.get("FileUtil.cs")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_NotIsMatchingFile_Null() {
        val config = getConfig()
        val settings = getSettings().copy(outExtensions = setOf("cs"))
        val finder = Finder(config, settings)
        val file = Paths.get("FileUtil.cs")
        assertNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_NonArchiveFileArchivesOnly_Null() {
        val config = getConfig()
        val settings = getSettings().copy(archivesOnly = true)
        val finder = Finder(config, settings)
        val file = Paths.get("FileUtil.cs")
        assertNull(finder.filterToFileResult(file))
    }

    /***************************************************************************
     * isMatchingDir tests
     **************************************************************************/
    @Test
    fun testIsMatchingDirPath_SingleDot_True() {
        val config = getConfig()
        val settings = getSettings()
        val finder = Finder(config, settings)
        assertTrue(finder.isMatchingDirPath(Paths.get(".")))
    }

    @Test
    fun testIsMatchingDirPath_DoubleDot_True() {
        val config = getConfig()
        val settings = getSettings()
        val finder = Finder(config, settings)
        assertTrue(finder.isMatchingDirPath(Paths.get("..")))
    }

    @Test
    fun testIsMatchingDirPath_IsHidden_False() {
        val config = getConfig()
        val settings = getSettings()
        val finder = Finder(config, settings)
        assertFalse(finder.isMatchingDirPath(Paths.get(".git")))
    }

    @Test
    fun testIsMatchingDirPath_IsHiddenIncludeHidden_True() {
        val config = getConfig()
        val settings = getSettings().copy(includeHidden = true)
        val finder = Finder(config, settings)
        assertTrue(finder.isMatchingDirPath(Paths.get(".git")))
    }

    @Test
    fun testIsMatchingDirPath_NoPatterns_True() {
        val config = getConfig()
        val settings = getSettings()
        val finder = Finder(config, settings)
        assertTrue(finder.isMatchingDirPath(Paths.get("/Users")))
    }

    @Test
    fun testIsMatchingDirPath_MatchesInPattern_True() {
        val config = getConfig()
        val settings = getSettings().copy(inDirPatterns = setOf(Regex("Find")))
        val finder = Finder(config, settings)
        assertTrue(finder.isMatchingDirPath(Paths.get("CsFind")))
    }

    @Test
    fun testIsMatchingDirPath_MatchesOutPattern_False() {
        val config = getConfig()
        val settings = getSettings().copy(outDirPatterns = setOf(Regex("Find")))
        val finder = Finder(config, settings)
        assertFalse(finder.isMatchingDirPath(Paths.get("CsFind")))
    }

    @Test
    fun testIsMatchingDirPath_DoesNotMatchInPattern_False() {
        val config = getConfig()
        val settings = getSettings().copy(inDirPatterns = setOf(Regex("FindFiles")))
        val finder = Finder(config, settings)
        assertFalse(finder.isMatchingDirPath(Paths.get("CsFind")))
    }

    @Test
    fun testIsMatchingDirPath_DoesNotMatchOutPattern_True() {
        val config = getConfig()
        val settings = getSettings().copy(outDirPatterns = setOf(Regex("FindFiles")))
        val finder = Finder(config, settings)
        val dir = Paths.get("CsFind")
        assertTrue(finder.isMatchingDirPath(dir))
    }

    /***************************************************************************
     * isMatchingFile tests
     **************************************************************************/
    @Test
    fun testIsMatchingFile_NoExtensionsNoPatterns_True() {
        val config = getConfig()
        val settings = getSettings()
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFile_MatchesInExtension_True() {
        val config = getConfig()
        val settings = getSettings().copy(inExtensions = setOf("cs"))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFile_DoesNotMatchInExtension_False() {
        val config = getConfig()
        val settings = getSettings().copy(inExtensions = setOf("java"))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("FileUtil.cs"), FileType.CODE)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFile_MatchesOutExtension_False() {
        val config = getConfig()
        val settings = getSettings().copy(outExtensions = setOf("cs"))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("FileUtil.cs"), FileType.CODE)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFile_DoesNotMatchOutExtension_True() {
        val config = getConfig()
        val settings = getSettings().copy(outExtensions = setOf("java"))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFile_MatchesInPattern_True() {
        val config = getConfig()
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Find")))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("Finder.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFile_DoesNotMatchInPattern_False() {
        val config = getConfig()
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Find")))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("FileUtil.cs"), FileType.CODE)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFile_MatchesOutPattern_False() {
        val config = getConfig()
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Find")))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("Finder.cs"), FileType.CODE)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFile_DoesNotMatchOutPattern_True() {
        val config = getConfig()
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Find")))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    /***************************************************************************
     * isMatchingArchiveFile tests
     **************************************************************************/
    @Test
    fun testIsMatchingArchiveFile_NoExtensionsNoPatterns_True() {
        val config = getConfig()
        val settings = getSettings()
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_MatchesInExtension_True() {
        val config = getConfig()
        val settings = getSettings().copy(inArchiveExtensions = setOf("zip"))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_DoesNotMatchInExtension_False() {
        val config = getConfig()
        val settings = getSettings().copy(inArchiveExtensions = setOf("gz"))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_MatchesOutExtension_False() {
        val config = getConfig()
        val settings = getSettings().copy(outArchiveExtensions = setOf("zip"))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_DoesNotMatchOutExtension_True() {
        val config = getConfig()
        val settings = getSettings().copy(outArchiveExtensions = setOf("gz"))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_MatchesInPattern_True() {
        val config = getConfig()
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("arch")))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_DoesNotMatchInPattern_False() {
        val config = getConfig()
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("archives")))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_MatchesOutPattern_False() {
        val config = getConfig()
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("arch")))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_DoesNotMatchOutPattern_True() {
        val config = getConfig()
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("archives")))
        val finder = Finder(config, settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }

    /*************************************************************
     * followSymlinks tests
     *************************************************************/
    @Test
    fun testFollowSymlinks_Default_Excluded() {
        val config = getConfig()
        var settings = getDefaultSettings().copy(paths = setOf(Paths.get(getBinPath())))
        var finder = Finder(config, settings)
        val fileResults: List<FileResult> = finder.find()
        assertTrue(fileResults.size < 4)
    }

    @Test
    fun testFollowSymlinks_FollowSymlinks_Included() {
        val config = getConfig()
        var settings = getDefaultSettings().copy(paths = setOf(Paths.get(getBinPath())), followSymlinks = true)
        var finder = Finder(config, settings)
        val fileResults: List<FileResult> = finder.find()
        assertTrue(fileResults.isEmpty() || fileResults.size > 2)
    }

    @Test
    fun testFollowSymlinks_NoFollowSymlinks_Excluded() {
        val config = getConfig()
        var settings = getDefaultSettings().copy(paths = setOf(Paths.get(getBinPath())), followSymlinks = false)
        var finder = Finder(config, settings)
        val fileResults: List<FileResult> = finder.find()
        assertTrue(fileResults.size < 4)
    }
}

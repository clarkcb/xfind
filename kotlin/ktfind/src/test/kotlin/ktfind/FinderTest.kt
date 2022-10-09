package ktfind

import java.io.File
import kotlin.test.*

/**
 * @author cary on 7/30/16.
 */
class FinderTest {
    private fun getSettings(): FindSettings {
        return getDefaultSettings().copy(paths=setOf("."))
    }

//    private val testFilePath = "/testFile2.txt"

    /***************************************************************************
     * filterToFileResult tests
     **************************************************************************/
    @Test
    fun testFilterToFileResult_IsHidden_Null() {
        val settings = getSettings()
        val finder = Finder(settings)
        val file = File(".gitignore")
        assertNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_IsHiddenIncludeHidden_NotNull() {
        val settings = getSettings().copy(excludeHidden = false)
        val finder = Finder(settings)
        val file = File(".gitignore")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_ArchiveNoIncludeArchives_Null() {
        val settings = getSettings()
        val finder = Finder(settings)
        val file = File("archive.zip")
        val fileResult = finder.filterToFileResult(file)
        assertNull(fileResult)
    }

    @Test
    fun testFilterToFileResult_ArchiveIncludeArchives_NotNull() {
        val settings = getSettings().copy(includeArchives = true)
        val finder = Finder(settings)
        val file = File("archive.zip")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_IsMatchingArchiveFile_NotNull() {
        val settings = getSettings().copy(includeArchives = true, inArchiveExtensions = setOf("zip"))
        val finder = Finder(settings)
        val file = File("archive.zip")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_NotIsMatchingArchiveFile_Null() {
        val settings = getSettings().copy(outExtensions = setOf("zip"))
        val finder = Finder(settings)
        val file = File("archive.zip")
        assertNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_ArchiveFileArchivesOnly_NotNull() {
        val settings = getSettings().copy(archivesOnly = true)
        val finder = Finder(settings)
        val file = File("archive.zip")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_NoExtensionsNoPatterns_NotNull() {
        val settings = getSettings()
        val finder = Finder(settings)
        val file = File("FileUtil.cs")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_IsMatchingFile_NotNull() {
        val settings = getSettings().copy(inExtensions = setOf("cs"))
        val finder = Finder(settings)
        val file = File("FileUtil.cs")
        assertNotNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_NotIsMatchingFile_Null() {
        val settings = getSettings().copy(outExtensions = setOf("cs"))
        val finder = Finder(settings)
        val file = File("FileUtil.cs")
        assertNull(finder.filterToFileResult(file))
    }

    @Test
    fun testFilterToFileResult_NonArchiveFileArchivesOnly_Null() {
        val settings = getSettings().copy(archivesOnly = true)
        val finder = Finder(settings)
        val file = File("FileUtil.cs")
        assertNull(finder.filterToFileResult(file))
    }

    /***************************************************************************
     * isMatchingDir tests
     **************************************************************************/
    @Test
    fun testIsMatchingDir_SingleDot_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        assertTrue(finder.isMatchingDir(File(".")))
    }

    @Test
    fun testIsMatchingDir_DoubleDot_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        assertTrue(finder.isMatchingDir(File("..")))
    }

    @Test
    fun testIsMatchingDir_IsHidden_False() {
        val settings = getSettings()
        val finder = Finder(settings)
        assertFalse(finder.isMatchingDir(File(".git")))
    }

    @Test
    fun testIsMatchingDir_IsHiddenIncludeHidden_True() {
        val settings = getSettings().copy(excludeHidden = false)
        val finder = Finder(settings)
        assertTrue(finder.isMatchingDir(File(".git")))
    }

    @Test
    fun testIsMatchingDir_NoPatterns_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        assertTrue(finder.isMatchingDir(File("/Users")))
    }

    @Test
    fun testIsMatchingDir_MatchesInPattern_True() {
        val settings = getSettings().copy(inDirPatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        assertTrue(finder.isMatchingDir(File("CsFind")))
    }

    @Test
    fun testIsMatchingDir_MatchesOutPattern_False() {
        val settings = getSettings().copy(outDirPatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        assertFalse(finder.isMatchingDir(File("CsFind")))
    }

    @Test
    fun testIsMatchingDir_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inDirPatterns = setOf(Regex("FindFiles")))
        val finder = Finder(settings)
        assertFalse(finder.isMatchingDir(File("CsFind")))
    }

    @Test
    fun testIsMatchingDir_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outDirPatterns = setOf(Regex("FindFiles")))
        val finder = Finder(settings)
        val dir = File("CsFind")
        assertTrue(finder.isMatchingDir(dir))
    }

    /***************************************************************************
     * isMatchingFile tests
     **************************************************************************/
    @Test
    fun testIsMatchingFile_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        val fileResult = FileResult(File("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFile(fileResult))
    }

    @Test
    fun testIsMatchingFile_MatchesInExtension_True() {
        val settings = getSettings().copy(inExtensions = setOf("cs"))
        val finder = Finder(settings)
        val fileResult = FileResult(File("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFile(fileResult))
    }

    @Test
    fun testIsMatchingFile_DoesNotMatchInExtension_False() {
        val settings = getSettings().copy(inExtensions = setOf("java"))
        val finder = Finder(settings)
        val fileResult = FileResult(File("FileUtil.cs"), FileType.CODE)
        assertFalse(finder.isMatchingFile(fileResult))
    }

    @Test
    fun testIsMatchingFile_MatchesOutExtension_False() {
        val settings = getSettings().copy(outExtensions = setOf("cs"))
        val finder = Finder(settings)
        val fileResult = FileResult(File("FileUtil.cs"), FileType.CODE)
        assertFalse(finder.isMatchingFile(fileResult))
    }

    @Test
    fun testIsMatchingFile_DoesNotMatchOutExtension_True() {
        val settings = getSettings().copy(outExtensions = setOf("java"))
        val finder = Finder(settings)
        val fileResult = FileResult(File("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFile(fileResult))
    }

    @Test
    fun testIsMatchingFile_MatchesInPattern_True() {
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val fileResult = FileResult(File("Finder.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFile(fileResult))
    }

    @Test
    fun testIsMatchingFile_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val fileResult = FileResult(File("FileUtil.cs"), FileType.CODE)
        assertFalse(finder.isMatchingFile(fileResult))
    }

    @Test
    fun testIsMatchingFile_MatchesOutPattern_False() {
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val fileResult = FileResult(File("Finder.cs"), FileType.CODE)
        assertFalse(finder.isMatchingFile(fileResult))
    }

    @Test
    fun testIsMatchingFile_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val fileResult = FileResult(File("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isMatchingFile(fileResult))
    }

    /***************************************************************************
     * isMatchingArchiveFile tests
     **************************************************************************/
    @Test
    fun testIsMatchingArchiveFile_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        val fileResult = FileResult(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFile(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_MatchesInExtension_True() {
        val settings = getSettings().copy(inArchiveExtensions = setOf("zip"))
        val finder = Finder(settings)
        val fileResult = FileResult(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFile(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_DoesNotMatchInExtension_False() {
        val settings = getSettings().copy(inArchiveExtensions = setOf("gz"))
        val finder = Finder(settings)
        val fileResult = FileResult(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFile(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_MatchesOutExtension_False() {
        val settings = getSettings().copy(outArchiveExtensions = setOf("zip"))
        val finder = Finder(settings)
        val fileResult = FileResult(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFile(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_DoesNotMatchOutExtension_True() {
        val settings = getSettings().copy(outArchiveExtensions = setOf("gz"))
        val finder = Finder(settings)
        val fileResult = FileResult(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFile(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_MatchesInPattern_True() {
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("arch")))
        val finder = Finder(settings)
        val fileResult = FileResult(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFile(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("archives")))
        val finder = Finder(settings)
        val fileResult = FileResult(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFile(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_MatchesOutPattern_False() {
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("arch")))
        val finder = Finder(settings)
        val fileResult = FileResult(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFile(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFile_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("archives")))
        val finder = Finder(settings)
        val fileResult = FileResult(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFile(fileResult))
    }
}

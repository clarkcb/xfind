package ktfind

import java.io.File
import java.nio.file.Paths
import kotlin.test.*

/**
 * @author cary on 7/30/16.
 */
class FinderTest {
    private fun getSettings(): FindSettings {
        return getDefaultSettings().copy(paths = setOf("."))
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
        val settings = getSettings().copy(includeHidden = true)
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
        val settings = getSettings().copy(includeHidden = true)
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
     * isMatchingFileResult tests
     **************************************************************************/
    @Test
    fun testIsMatchingFileResult_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_MatchesInExtension_True() {
        val settings = getSettings().copy(inExtensions = setOf("cs"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_DoesNotMatchInExtension_False() {
        val settings = getSettings().copy(inExtensions = setOf("java"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_MatchesOutExtension_False() {
        val settings = getSettings().copy(outExtensions = setOf("cs"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_DoesNotMatchOutExtension_True() {
        val settings = getSettings().copy(outExtensions = setOf("java"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_MatchesInPattern_True() {
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val path = Paths.get("Finder.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_MatchesOutPattern_False() {
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val path = Paths.get("Finder.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_MatchesInMimeType_True() {
        val settings = getSettings().copy(inMimeTypes = setOf("text/plain"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_DoesNotMatchInMimeType_False() {
        val settings = getSettings().copy(inMimeTypes = setOf("application/json"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_MatchesInMimeTypeWildCard_True() {
        val settings = getSettings().copy(inMimeTypes = setOf("text/*"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_DoesNotMatchInMimeTypeWildCard_False() {
        val settings = getSettings().copy(inMimeTypes = setOf("application/*"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_MatchesOutMimeType_False() {
        val settings = getSettings().copy(outMimeTypes = setOf("text/plain"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_DoesNotMatchOutMimeType_True() {
        val settings = getSettings().copy(outMimeTypes = setOf("application/json"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_MatchesOutMimeTypeWildCard_False() {
        val settings = getSettings().copy(outMimeTypes = setOf("text/*"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    fun testIsMatchingFileResult_DoesNotMatchOutMimeTypeWildCard_True() {
        val settings = getSettings().copy(outMimeTypes = setOf("application/*"))
        val finder = Finder(settings)
        val path = Paths.get("FileUtil.cs")
        val fileType = FileType.CODE
        val mimeType = "text/plain"
        val fileResult = FileResult(path, fileType, mimeType)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    /***************************************************************************
     * isMatchingArchiveFileResult tests
     **************************************************************************/
    @Test
    fun testIsMatchingArchiveFileResult_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFileResult_MatchesInExtension_True() {
        val settings = getSettings().copy(inArchiveExtensions = setOf("zip"))
        val finder = Finder(settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFileResult_DoesNotMatchInExtension_False() {
        val settings = getSettings().copy(inArchiveExtensions = setOf("gz"))
        val finder = Finder(settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFileResult_MatchesOutExtension_False() {
        val settings = getSettings().copy(outArchiveExtensions = setOf("zip"))
        val finder = Finder(settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFileResult_DoesNotMatchOutExtension_True() {
        val settings = getSettings().copy(outArchiveExtensions = setOf("gz"))
        val finder = Finder(settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFileResult_MatchesInPattern_True() {
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("arch")))
        val finder = Finder(settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFileResult_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("archives")))
        val finder = Finder(settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFileResult_MatchesOutPattern_False() {
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("arch")))
        val finder = Finder(settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isMatchingArchiveFileResult(fileResult))
    }

    @Test
    fun testIsMatchingArchiveFileResult_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("archives")))
        val finder = Finder(settings)
        val fileResult = FileResult(Paths.get("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isMatchingArchiveFileResult(fileResult))
    }
}

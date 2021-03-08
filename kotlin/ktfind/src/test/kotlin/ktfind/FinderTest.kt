package ktfind

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import java.io.File

/**
 * @author cary on 7/30/16.
 */
class FinderTest {
    private fun getSettings(): FindSettings {
        return getDefaultSettings().copy(paths=setOf("."))
    }

    private val testFilePath = "/testFile2.txt"

    /***************************************************************************
     * filterFile tests
     **************************************************************************/
    @Test
    fun testFilterFile_IsHidden_False() {
        val settings = getSettings()
        val finder = Finder(settings)
        val file = File(".gitignore")
        assertEquals(null, finder.filterToFindFile(file))
    }

    @Test
    fun testFilterFile_IsHiddenIncludeHidden_True() {
        val settings = getSettings().copy(excludeHidden = false)
        val finder = Finder(settings)
        val file = File(".gitignore")
        // val findFile = FindFile(file, FileType.TEXT)
        // assertEquals(findFile, finder.filterToFindFile(file))
        assertEquals(".gitignore", finder.filterToFindFile(file)!!.file.name)
    }

    @Test
    fun testFilterFile_ArchiveNoIncludeArchives_False() {
        val settings = getSettings()
        val finder = Finder(settings)
        val file = File("archive.zip")
        val filteredFile = finder.filterToFindFile(file)
        assertEquals(null, filteredFile)
    }

    @Test
    fun testFilterFile_ArchiveIncludeArchives_True() {
        val settings = getSettings().copy(includeArchives = true)
        val finder = Finder(settings)
        val file = File("archive.zip")
        val findFile = FindFile(file, FileType.ARCHIVE)
        val filteredFile = finder.filterToFindFile(file)
        assertEquals(findFile.file.name, filteredFile!!.file.name)
        assertEquals(FileType.ARCHIVE, filteredFile.fileType)
    }

    @Test
    fun testFilterFile_IsArchiveFindFile_True() {
        val settings = getSettings().copy(includeArchives = true, inArchiveExtensions = setOf("zip"))
        val finder = Finder(settings)
        val file = File("archive.zip")
        val findFile = FindFile(file, FileType.ARCHIVE)
        val filteredFile = finder.filterToFindFile(file)
        assertEquals(findFile.file.name, filteredFile!!.file.name)
        assertEquals(FileType.ARCHIVE, filteredFile.fileType)
    }

    @Test
    fun testFilterFile_NotIsArchiveFindFile_False() {
        val settings = getSettings().copy(outExtensions = setOf("zip"))
        val finder = Finder(settings)
        val file = File("archive.zip")
        assertEquals(null, finder.filterToFindFile(file))
    }

    @Test
    fun testFilterFile_ArchiveFileArchivesOnly_True() {
        val settings = getSettings().copy(archivesOnly = true)
        val finder = Finder(settings)
        val file = File("archive.zip")
        val findFile = FindFile(file, FileType.ARCHIVE)
        val filteredFile = finder.filterToFindFile(file)
        assertEquals(findFile.file.name, filteredFile!!.file.name)
        assertEquals(FileType.ARCHIVE, filteredFile.fileType)
    }

    @Test
    fun testFilterFile_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        val file = File("FileUtil.cs")
        val findFile = FindFile(file, FileType.CODE)
        val filteredFile = finder.filterToFindFile(file)
        assertEquals(findFile.file.name, filteredFile!!.file.name)
        assertEquals(FileType.CODE, filteredFile.fileType)
    }

    @Test
    fun testFilterFile_IsFindFile_True() {
        val settings = getSettings().copy(inExtensions = setOf("cs"))
        val finder = Finder(settings)
        val file = File("FileUtil.cs")
        val findFile = FindFile(file, FileType.CODE)
        val filteredFile = finder.filterToFindFile(file)
        assertEquals(findFile.file.name, filteredFile!!.file.name)
        assertEquals(FileType.CODE, filteredFile.fileType)
    }

    @Test
    fun testFilterFile_NotIsFindFile_False() {
        val settings = getSettings().copy(outExtensions = setOf("cs"))
        val finder = Finder(settings)
        val file = File("FileUtil.cs")
        assertEquals(null, finder.filterToFindFile(file))
    }

    @Test
    fun testFilterFile_NonArchiveFileArchivesOnly_False() {
        val settings = getSettings().copy(archivesOnly = true)
        val finder = Finder(settings)
        val file = File("FileUtil.cs")
        assertEquals(null, finder.filterToFindFile(file))
    }

    /***************************************************************************
     * isFindDir tests
     **************************************************************************/
    @Test
    fun testisFindDir_SingleDot_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        assertTrue(finder.isFindDir(File(".")))
    }

    @Test
    fun testisFindDir_DoubleDot_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        assertTrue(finder.isFindDir(File("..")))
    }

    @Test
    fun testisFindDir_IsHidden_False() {
        val settings = getSettings()
        val finder = Finder(settings)
        assertFalse(finder.isFindDir(File(".git")))
    }

    @Test
    fun testisFindDir_IsHiddenIncludeHidden_True() {
        val settings = getSettings().copy(excludeHidden = false)
        val finder = Finder(settings)
        assertTrue(finder.isFindDir(File(".git")))
    }

    @Test
    fun testisFindDir_NoPatterns_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        assertTrue(finder.isFindDir(File("/Users")))
    }

    @Test
    fun testisFindDir_MatchesInPattern_True() {
        val settings = getSettings().copy(inDirPatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        assertTrue(finder.isFindDir(File("CsFind")))
    }

    @Test
    fun testisFindDir_MatchesOutPattern_False() {
        val settings = getSettings().copy(outDirPatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        assertFalse(finder.isFindDir(File("CsFind")))
    }

    @Test
    fun testisFindDir_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inDirPatterns = setOf(Regex("FindFiles")))
        val finder = Finder(settings)
        assertFalse(finder.isFindDir(File("CsFind")))
    }

    @Test
    fun testisFindDir_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outDirPatterns = setOf(Regex("FindFiles")))
        val finder = Finder(settings)
        val dir = File("CsFind")
        assertTrue(finder.isFindDir(dir))
    }

    /***************************************************************************
     * isFindFile tests
     **************************************************************************/
    @Test
    fun testIsFindFile_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        val file = FindFile(File("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isFindFile(file))
    }

    @Test
    fun testIsFindFile_MatchesInExtension_True() {
        val settings = getSettings().copy(inExtensions = setOf("cs"))
        val finder = Finder(settings)
        val file = FindFile(File("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isFindFile(file))
    }

    @Test
    fun testIsFindFile_DoesNotMatchInExtension_False() {
        val settings = getSettings().copy(inExtensions = setOf("java"))
        val finder = Finder(settings)
        val file = FindFile(File("FileUtil.cs"), FileType.CODE)
        assertFalse(finder.isFindFile(file))
    }

    @Test
    fun testIsFindFile_MatchesOutExtension_False() {
        val settings = getSettings().copy(outExtensions = setOf("cs"))
        val finder = Finder(settings)
        val file = FindFile(File("FileUtil.cs"), FileType.CODE)
        assertFalse(finder.isFindFile(file))
    }

    @Test
    fun testIsFindFile_DoesNotMatchOutExtension_True() {
        val settings = getSettings().copy(outExtensions = setOf("java"))
        val finder = Finder(settings)
        val file = FindFile(File("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isFindFile(file))
    }

    @Test
    fun testIsFindFile_MatchesInPattern_True() {
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val file = FindFile(File("Finder.cs"), FileType.CODE)
        assertTrue(finder.isFindFile(file))
    }

    @Test
    fun testIsFindFile_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val file = FindFile(File("FileUtil.cs"), FileType.CODE)
        assertFalse(finder.isFindFile(file))
    }

    @Test
    fun testIsFindFile_MatchesOutPattern_False() {
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val file = FindFile(File("Finder.cs"), FileType.CODE)
        assertFalse(finder.isFindFile(file))
    }

    @Test
    fun testIsFindFile_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Find")))
        val finder = Finder(settings)
        val file = FindFile(File("FileUtil.cs"), FileType.CODE)
        assertTrue(finder.isFindFile(file))
    }

    /***************************************************************************
     * isArchiveFindFile tests
     **************************************************************************/
    @Test
    fun testIsArchiveFindFile_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val finder = Finder(settings)
        val file = FindFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isArchiveFindFile(file))
    }

    @Test
    fun testIsArchiveFindFile_MatchesInExtension_True() {
        val settings = getSettings().copy(inArchiveExtensions = setOf("zip"))
        val finder = Finder(settings)
        val file = FindFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isArchiveFindFile(file))
    }

    @Test
    fun testIsArchiveFindFile_DoesNotMatchInExtension_False() {
        val settings = getSettings().copy(inArchiveExtensions = setOf("gz"))
        val finder = Finder(settings)
        val file = FindFile(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isArchiveFindFile(file))
    }

    @Test
    fun testIsArchiveFindFile_MatchesOutExtension_False() {
        val settings = getSettings().copy(outArchiveExtensions = setOf("zip"))
        val finder = Finder(settings)
        val file = FindFile(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isArchiveFindFile(file))
    }

    @Test
    fun testIsArchiveFindFile_DoesNotMatchOutExtension_True() {
        val settings = getSettings().copy(outArchiveExtensions = setOf("gz"))
        val finder = Finder(settings)
        val file = FindFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isArchiveFindFile(file))
    }

    @Test
    fun testIsArchiveFindFile_MatchesInPattern_True() {
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("arch")))
        val finder = Finder(settings)
        val file = FindFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isArchiveFindFile(file))
    }

    @Test
    fun testIsArchiveFindFile_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("archives")))
        val finder = Finder(settings)
        val file = FindFile(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isArchiveFindFile(file))
    }

    @Test
    fun testIsArchiveFindFile_MatchesOutPattern_False() {
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("arch")))
        val finder = Finder(settings)
        val file = FindFile(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(finder.isArchiveFindFile(file))
    }

    @Test
    fun testIsArchiveFindFile_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("archives")))
        val finder = Finder(settings)
        val file = FindFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(finder.isArchiveFindFile(file))
    }
}

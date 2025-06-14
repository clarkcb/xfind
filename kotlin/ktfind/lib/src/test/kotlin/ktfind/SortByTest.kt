package ktfind

import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * @author cary on 7/30/16.
 */
class SortByTest {

    /***************************************************************************
     * fromName tests
     **************************************************************************/
    @Test
    fun testFromNameFileNameAllUppercase() {
        val sortByName = "FILENAME"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILENAME, sortBy)
    }

    @Test
    fun testFromNameFileNameAllLowercase() {
        val sortByName = "filename"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILENAME, sortBy)
    }

    @Test
    fun testFromNameName() {
        val sortByName = "NAME"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILENAME, sortBy)
    }

    @Test
    fun testFromNameFileSizeAllUppercase() {
        val sortByName = "FILESIZE"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILESIZE, sortBy)
    }

    @Test
    fun testFromNameFileSizeAllLowercase() {
        val sortByName = "filesize"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILESIZE, sortBy)
    }

    @Test
    fun testFromNameSize() {
        val sortByName = "size"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILESIZE, sortBy)
    }

    @Test
    fun testFromNameFileTypeAllUppercase() {
        val sortByName = "FILETYPE"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILETYPE, sortBy)
    }

    @Test
    fun testFromNameFileTypeAllLowercase() {
        val sortByName = "filetype"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILETYPE, sortBy)
    }

    @Test
    fun testFromNameType() {
        val sortByName = "type"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILETYPE, sortBy)
    }

    @Test
    fun testFromNameLastModAllUppercase() {
        val sortByName = "LASTMOD"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.LASTMOD, sortBy)
    }

    @Test
    fun testFromNameLastModAllLowercase() {
        val sortByName = "lastmod"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.LASTMOD, sortBy)
    }

    @Test
    fun testFromNameFilePathAllUppercase() {
        val sortByName = "FILEPATH"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILEPATH, sortBy)
    }

    @Test
    fun testFromNameFilePathAllLowercase() {
        val sortByName = "filepath"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILEPATH, sortBy)
    }

    @Test
    fun testFromNameAnything() {
        val sortByName = "anything"
        val sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILEPATH, sortBy)
    }
}

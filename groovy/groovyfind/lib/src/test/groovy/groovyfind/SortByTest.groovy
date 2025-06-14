package groovyfind

import org.junit.jupiter.api.Test

import static org.junit.jupiter.api.Assertions.assertEquals

class SortByTest {

    SortByTest() {}

    /***************************************************************************
     * fromName tests
     **************************************************************************/
    @Test
    final void testFromNameFileNameAllUppercase() {
        var sortByName = 'FILENAME'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILENAME, sortBy)
    }

    @Test
    final void testFromNameFileNameAllLowercase() {
        var sortByName = 'filename'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILENAME, sortBy)
    }

    @Test
    final void testFromNameName() {
        var sortByName = 'NAME'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILENAME, sortBy)
    }

    @Test
    final void testFromNameFileSizeAllUppercase() {
        var sortByName = 'FILESIZE'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILESIZE, sortBy)
    }

    @Test
    final void testFromNameFileSizeAllLowercase() {
        var sortByName = 'filesize'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILESIZE, sortBy)
    }

    @Test
    final void testFromNameSize() {
        var sortByName = 'size'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILESIZE, sortBy)
    }

    @Test
    final void testFromNameFileTypeAllUppercase() {
        var sortByName = 'FILETYPE'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILETYPE, sortBy)
    }

    @Test
    final void testFromNameFileTypeAllLowercase() {
        var sortByName = 'filetype'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILETYPE, sortBy)
    }

    @Test
    final void testFromNameType() {
        var sortByName = 'type'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILETYPE, sortBy)
    }

    @Test
    final void testFromNameLastModAllUppercase() {
        var sortByName = 'LASTMOD'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.LASTMOD, sortBy)
    }

    @Test
    final void testFromNameLastModAllLowercase() {
        var sortByName = 'lastmod'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.LASTMOD, sortBy)
    }

    @Test
    final void testFromNameFilePathAllUppercase() {
        var sortByName = 'FILEPATH'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILEPATH, sortBy)
    }

    @Test
    final void testFromNameFilePathAllLowercase() {
        var sortByName = 'filepath'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILEPATH, sortBy)
    }

    @Test
    final void testFromNameAnything() {
        var sortByName = 'anything'
        var sortBy = SortBy.forName(sortByName)
        assertEquals(SortBy.FILEPATH, sortBy)
    }
}

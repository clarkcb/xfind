package groovyfind

import org.junit.jupiter.api.Test

import java.nio.file.Paths

import static org.junit.jupiter.api.Assertions.*

class FileUtilTest {

    FileUtilTest() {}

    /***************************************************************************
     * expandPath tests
     **************************************************************************/
    @Test
    final void testExpandPathTilde() {
        var tildePath = Paths.get("~")
        var userPath = Paths.get(System.getProperty("user.home"))
        assertEquals(userPath, FileUtil.expandPath(tildePath))
    }

    @Test
    final void testExpandPathTildePath() {
        var tildePath = Paths.get("~/src/xfind")
        var expandedPath = Paths.get(System.getProperty("user.home"), "src/xfind")
        assertEquals(expandedPath, FileUtil.expandPath(tildePath))
    }

    @Test
    final void testExpandPathTildeNamePath() {
        var tildePath = Paths.get("~cary/src/xfind")
        var expandedPath = Paths.get(System.getProperty("user.home"), "src/xfind")
        assertEquals(expandedPath, FileUtil.expandPath(tildePath))
    }

    /***************************************************************************
     * getExtension tests
     **************************************************************************/
    @Test
    final void testGetTxtExtension() {
        var path = Paths.get('filename.txt')
        assertEquals('txt', FileUtil.getExtension(path))
    }

    @Test
    final void testGetMissingExtension() {
        var path = Paths.get('filename.')
        assertEquals('', FileUtil.getExtension(path))
    }

    @Test
    final void testGetNoExtension() {
        var path = Paths.get('filename')
        assertEquals('', FileUtil.getExtension(path))
    }

    @Test
    final void testGetHiddenTxtExtension() {
        var path = Paths.get('.filename.txt')
        assertEquals('txt', FileUtil.getExtension(path))
    }

    @Test
    final void testGetHiddenMissingExtension() {
        var path = Paths.get('.filename.')
        assertEquals('', FileUtil.getExtension(path))
    }

    @Test
    final void testGetHiddenNoExtension() {
        var path = Paths.get('.filename')
        assertEquals('', FileUtil.getExtension(path))
    }

    /***************************************************************************
     * isDotDir tests
     **************************************************************************/
    @Test
    final void testIsDotDirSingleDot() {
        var filename = '.'
        assertTrue(FileUtil.isDotDir(filename))
    }

    @Test
    final void testIsDotDirDoubleDot() {
        var filename = '..'
        assertTrue(FileUtil.isDotDir(filename))
    }

    @Test
    final void testIsDotDirNotDotDir() {
        var filename = '~/path'
        assertFalse(FileUtil.isDotDir(filename))
    }

    @Test
    final void testIsDotDirPathWithDot() {
        var filename = './path'
        assertFalse(FileUtil.isDotDir(filename))
    }

    @Test
    final void testIsDotDirHiddenFile() {
        var filename = '.gitignore'
        assertFalse(FileUtil.isDotDir(filename))
    }

    /***************************************************************************
     * isHidden tests
     **************************************************************************/
    @Test
    final void testIsHiddenSingleDot() {
        var filename = '.'
        assertFalse(FileUtil.isHidden(filename))
    }

    @Test
    final void testIsHiddenDoubleDot() {
        var filename = '..'
        assertFalse(FileUtil.isHidden(filename))
    }

    @Test
    final void testIsHiddenHiddenFileName() {
        var filename = '.gitignore'
        assertTrue(FileUtil.isHidden(filename))
    }

    @Test
    final void testIsHiddenNotHiddenFileName() {
        var filename = 'file.txt'
        assertFalse(FileUtil.isHidden(filename))
    }

    @Test
    final void testIsHiddenHiddenFile() {
        var path = Paths.get('.gitignore')
        assertTrue(FileUtil.isHidden(path))
    }

    @Test
    final void testIsHiddenNotHiddenFile() {
        var path = Paths.get('./file.txt')
        assertFalse(FileUtil.isHidden(path))
    }

    /***************************************************************************
     * splitPath tests
     **************************************************************************/
    @Test
    final void testSplitPathWithDot() {
        var path = './path/to/somewhere/'
        var elems = FileUtil.splitPath(path)
        assertEquals(elems.size(), 4)
        assertEquals('.', elems.get(0))
        assertEquals('path', elems.get(1))
    }

    @Test
    final void testSplitPathWithDoubleDot() {
        var path = '../path/to/somewhere/'
        var elems = FileUtil.splitPath(path)
        assertEquals(elems.size(), 4)
        assertEquals('..', elems.get(0))
        assertEquals('path', elems.get(1))
    }

    @Test
    final void testSplitPathWithoutDot() {
        var path = '/path/to/somewhere/'
        var elems = FileUtil.splitPath(path)
        assertEquals(elems.size(), 3)
        assertEquals('path', elems.get(0))
    }

}

package ktfind

import java.nio.file.Paths
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * @author cary on 7/30/16.
 */
class FileUtilTest {

    /***************************************************************************
     * expandPath tests
     **************************************************************************/
    @Test
    fun testExpandPathTilde() {
        val tildePath = Paths.get("~")
        val userPath = Paths.get(System.getProperty("user.home"))
        assertEquals(userPath, FileUtil.expandPath(tildePath))
    }

    @Test
    fun testExpandPathTildePath() {
        val tildePath = Paths.get("~/src/xfind")
        val userPath = Paths.get(System.getProperty("user.home"), "src/xfind")
        assertEquals(userPath, FileUtil.expandPath(tildePath))
    }

    @Test
    fun testExpandPathTildeNamePath() {
        val tildePath = Paths.get("~cary/src/xfind")
        val userPath = Paths.get(System.getProperty("user.home"), "src/xfind")
        assertEquals(userPath, FileUtil.expandPath(tildePath))
    }

    /***************************************************************************
     * isDotDir Path tests
     **************************************************************************/
    @Test
    fun testIsDotDirPathSingleDot() {
        val path = Paths.get(".")
        assertTrue(FileUtil.isDotDir(path))
    }

    @Test
    fun testIsDotDirPathDoubleDot() {
        val path = Paths.get("..")
        assertTrue(FileUtil.isDotDir(path))
    }

    @Test
    fun testIsDotDirPathNotDotDir() {
        val path = Paths.get("~/path")
        assertFalse(FileUtil.isDotDir(path))
    }

    @Test
    fun testIsDotDirPathWithDot() {
        val path = Paths.get("./path")
        assertFalse(FileUtil.isDotDir(path))
    }

    @Test
    fun testIsDotDirPathHidden() {
        val path = Paths.get(".gitignore")
        assertFalse(FileUtil.isDotDir(path))
    }

    /***************************************************************************
     * isDotDir fileName tests
     **************************************************************************/
    @Test
    fun testIsDotDirFileNameSingleDot() {
        val fileName = "."
        assertTrue(FileUtil.isDotDir(fileName))
    }

    @Test
    fun testIsDotDirFileNameDoubleDot() {
        val fileName = ".."
        assertTrue(FileUtil.isDotDir(fileName))
    }

    @Test
    fun testIsDotDirFileNameNotDotDir() {
        val fileName = "~/path"
        assertFalse(FileUtil.isDotDir(fileName))
    }

    @Test
    fun testIsDotDirFileNameWithDot() {
        val fileName = "./path"
        assertFalse(FileUtil.isDotDir(fileName))
    }

    @Test
    fun testIsDotDirFileNameHidden() {
        val fileName = ".gitignore"
        assertFalse(FileUtil.isDotDir(fileName))
    }

    /***************************************************************************
     * isHidden Path tests
     **************************************************************************/
    @Test
    fun testIsHiddenPathSingleDot() {
        val path = Paths.get(".")
        assertFalse(FileUtil.isHidden(path))
    }

    @Test
    fun testIsHiddenPathDoubleDot() {
        val path = Paths.get("..")
        assertFalse(FileUtil.isHidden(path))
    }

    @Test
    fun testIsHiddenPathHidden() {
        val path = Paths.get("./.gitignore")
        assertTrue(FileUtil.isHidden(path))
    }

    @Test
    fun testIsHiddenPathNotHidden() {
        val path = Paths.get("./file.txt")
        assertFalse(FileUtil.isHidden(path))
    }

    /***************************************************************************
     * isHidden fileName tests
     **************************************************************************/
    @Test
    fun testIsHiddenFileNameSingleDot() {
        val fileName = "."
        assertFalse(FileUtil.isHidden(fileName))
    }

    @Test
    fun testIsHiddenFileNameDoubleDot() {
        val fileName = ".."
        assertFalse(FileUtil.isHidden(fileName))
    }

    @Test
    fun testIsHiddenFileNameHidden() {
        val fileName = ".gitignore"
        assertTrue(FileUtil.isHidden(fileName))
    }

    @Test
    fun testIsHiddenFileNameNotHidden() {
        val fileName = "file.txt"
        assertFalse(FileUtil.isHidden(fileName))
    }
}

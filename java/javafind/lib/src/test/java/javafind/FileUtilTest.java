package javafind;

import org.junit.jupiter.api.Test;

import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

public class FileUtilTest {
    public FileUtilTest() {}

    /***************************************************************************
     * expandPath tests
     **************************************************************************/
    @Test
    public final void testExpandPathTilde() {
        var tildePath = Paths.get("~");
        var userPath = Paths.get(System.getProperty("user.home"));
        assertEquals(userPath, FileUtil.expandPath(tildePath));
    }

    @Test
    public final void testExpandPathTildePath() {
        var tildePath = Paths.get("~/src/xfind");
        var expandedPath = Paths.get(System.getProperty("user.home"), "src/xfind");
        assertEquals(expandedPath, FileUtil.expandPath(tildePath));
    }

    @Test
    public final void testExpandPathTildeNamePath() {
        var tildePath = Paths.get("~cary/src/xfind");
        var expandedPath = Paths.get(System.getProperty("user.home"), "src/xfind");
        assertEquals(expandedPath, FileUtil.expandPath(tildePath));
    }

    /***************************************************************************
     * getExtension tests
     **************************************************************************/
    @Test
    public final void testGetTxtExtension() {
        var path = Paths.get("filename.txt");
        assertEquals("txt", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetMissingExtension() {
        var path = Paths.get("filename.");
        assertEquals("", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetNoExtension() {
        var path = Paths.get("filename");
        assertEquals("", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetHiddenTxtExtension() {
        var path = Paths.get(".filename.txt");
        assertEquals("txt", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetHiddenMissingExtension() {
        var path = Paths.get(".filename.");
        assertEquals("", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetHiddenNoExtension() {
        var path = Paths.get(".filename");
        assertEquals("", FileUtil.getExtension(path));
    }

    /***************************************************************************
     * isDotDir tests
     **************************************************************************/
    @Test
    public final void testIsDotDirSingleDot() {
        var filename = ".";
        assertTrue(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirDoubleDot() {
        var filename = "..";
        assertTrue(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirNotDotDir() {
        var filename = "~/path";
        assertFalse(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirPathWithDot() {
        var filename = "./path";
        assertFalse(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirHiddenFile() {
        var filename = ".gitignore";
        assertFalse(FileUtil.isDotDir(filename));
    }

    /***************************************************************************
     * isHiddenName tests
     **************************************************************************/
    @Test
    public final void testIsHiddenNameSingleDot() {
        var filename = ".";
        assertFalse(FileUtil.isHiddenName(filename));
    }

    @Test
    public final void testIsHiddenNameDoubleDot() {
        var filename = "..";
        assertFalse(FileUtil.isHiddenName(filename));
    }

    @Test
    public final void testIsHiddenNameHiddenFileName() {
        var filename = ".gitignore";
        assertTrue(FileUtil.isHiddenName(filename));
    }

    @Test
    public final void testIsHiddenNameNotHiddenFileName() {
        var filename = "file.txt";
        assertFalse(FileUtil.isHiddenName(filename));
    }

    /***************************************************************************
     * isHiddenPath tests
     **************************************************************************/
    @Test
    public final void testIsHiddenPathSingleDot() {
        var path = Paths.get(".");
        assertFalse(FileUtil.isHiddenPath(path));
    }

    @Test
    public final void testIsHiddenPathDoubleDot() {
        var path = Paths.get("..");
        assertFalse(FileUtil.isHiddenPath(path));
    }

    @Test
    public final void testIsHiddenPathHiddenFileName() {
        var path = Paths.get(".gitignore");
        assertTrue(FileUtil.isHiddenPath(path));
    }

    @Test
    public final void testIsHiddenPathNotHiddenFileName() {
        var path = Paths.get("./file.txt");
        assertFalse(FileUtil.isHiddenPath(path));
    }

    /***************************************************************************
     * splitPath tests
     **************************************************************************/
    @Test
    public final void testSplitPathWithDot() {
        var path = "./path/to/somewhere/";
        var elems = FileUtil.splitPath(path);
        assertEquals(4, elems.size());
        assertEquals(".", elems.get(0));
        assertEquals("path", elems.get(1));
    }

    @Test
    public final void testSplitPathWithDoubleDot() {
        var path = "../path/to/somewhere/";
        var elems = FileUtil.splitPath(path);
        assertEquals(4, elems.size());
        assertEquals("..", elems.get(0));
        assertEquals("path", elems.get(1));
    }

    @Test
    public final void testSplitPathWithoutDot() {
        var path = "/path/to/somewhere/";
        var elems = FileUtil.splitPath(path);
        assertEquals(3, elems.size());
        assertEquals("path", elems.get(0));
    }
}

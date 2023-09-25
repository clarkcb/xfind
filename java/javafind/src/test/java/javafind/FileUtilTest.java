package javafind;

import org.junit.Test;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.junit.Assert.*;

public class FileUtilTest {
    public FileUtilTest() {}

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
     * isHidden tests
     **************************************************************************/
    @Test
    public final void testIsHiddenSingleDot() {
        var filename = ".";
        assertFalse(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenDoubleDot() {
        var filename = "..";
        assertFalse(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenHiddenFileName() {
        var filename = ".gitignore";
        assertTrue(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenNotHiddenFileName() {
        var filename = "file.txt";
        assertFalse(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenHiddenFile() {
        var path = Paths.get(".gitignore");
        assertTrue(FileUtil.isHidden(path));
    }

    @Test
    public final void testIsHiddenNotHiddenFile() {
        var path = Paths.get("./file.txt");
        assertFalse(FileUtil.isHidden(path));
    }

    /***************************************************************************
     * splitPath tests
     **************************************************************************/
    @Test
    public final void testSplitPathWithDot() {
        var path = "./path/to/somewhere/";
        var elems = FileUtil.splitPath(path);
        assertEquals(elems.size(), 4);
        assertEquals(".", elems.get(0));
        assertEquals("path", elems.get(1));
    }

    @Test
    public final void testSplitPathWithDoubleDot() {
        var path = "../path/to/somewhere/";
        var elems = FileUtil.splitPath(path);
        assertEquals(elems.size(), 4);
        assertEquals("..", elems.get(0));
        assertEquals("path", elems.get(1));
    }

    @Test
    public final void testSplitPathWithoutDot() {
        var path = "/path/to/somewhere/";
        var elems = FileUtil.splitPath(path);
        assertEquals(elems.size(), 3);
        assertEquals("path", elems.get(0));
    }
}

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
        Path path = Paths.get("filename.txt");
        assertEquals("txt", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetMissingExtension() {
        Path path = Paths.get("filename.");
        assertEquals("", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetNoExtension() {
        Path path = Paths.get("filename");
        assertEquals("", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetHiddenTxtExtension() {
        Path path = Paths.get(".filename.txt");
        assertEquals("txt", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetHiddenMissingExtension() {
        Path path = Paths.get(".filename.");
        assertEquals("", FileUtil.getExtension(path));
    }

    @Test
    public final void testGetHiddenNoExtension() {
        Path path = Paths.get(".filename");
        assertEquals("", FileUtil.getExtension(path));
    }

    /***************************************************************************
     * isDotDir tests
     **************************************************************************/
    @Test
    public final void testIsDotDirSingleDot() {
        String filename = ".";
        assertTrue(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirDoubleDot() {
        String filename = "..";
        assertTrue(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirNotDotDir() {
        String filename = "~/path";
        assertFalse(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirPathWithDot() {
        String filename = "./path";
        assertFalse(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirHiddenFile() {
        String filename = ".gitignore";
        assertFalse(FileUtil.isDotDir(filename));
    }

    /***************************************************************************
     * isHidden tests
     **************************************************************************/
    @Test
    public final void testIsHiddenSingleDot() {
        String filename = ".";
        assertFalse(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenDoubleDot() {
        String filename = "..";
        assertFalse(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenHiddenFileName() {
        String filename = ".gitignore";
        assertTrue(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenNotHiddenFileName() {
        String filename = "file.txt";
        assertFalse(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenHiddenFile() {
        Path path = Paths.get(".gitignore");
        assertTrue(FileUtil.isHidden(path));
    }

    @Test
    public final void testIsHiddenNotHiddenFile() {
        Path path = Paths.get("./file.txt");
        assertFalse(FileUtil.isHidden(path));
    }

    /***************************************************************************
     * splitPath tests
     **************************************************************************/
    @Test
    public final void testSplitPathWithDot() {
        String path = "./path/to/somewhere/";
        List<String> elems = FileUtil.splitPath(path);
        assertEquals(elems.size(), 4);
        assertEquals(".", elems.get(0));
        assertEquals("path", elems.get(1));
    }

    @Test
    public final void testSplitPathWithDoubleDot() {
        String path = "../path/to/somewhere/";
        List<String> elems = FileUtil.splitPath(path);
        assertEquals(elems.size(), 4);
        assertEquals("..", elems.get(0));
        assertEquals("path", elems.get(1));
    }

    @Test
    public final void testSplitPathWithoutDot() {
        String path = "/path/to/somewhere/";
        List<String> elems = FileUtil.splitPath(path);
        assertEquals(elems.size(), 3);
        assertEquals("path", elems.get(0));
    }
}

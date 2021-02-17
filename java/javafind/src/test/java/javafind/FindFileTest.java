package javafind;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class FindFileTest {

    public FindFileTest() {}

    @Test
    public final void test_findfile_abs_path() {
        String path = "~/src/xfind/java/javafind/src/main/java/javafind";
        String filename = "FindFile.java";
        FindFile findFile = new FindFile(path, filename, FileType.CODE);
        String expectedFilePath = path + "/" + filename;
        assertEquals(expectedFilePath, findFile.toString());
    }

    @Test
    public final void test_findfile_rel_path1() {
        String path = ".";
        String filename = "FindFile.java";
        FindFile findFile = new FindFile(path, filename, FileType.CODE);
        String expectedFilePath = path + "/" + filename;
        assertEquals(expectedFilePath, findFile.toString());
    }

    @Test
    public final void test_findfile_rel_path2() {
        String path = "..";
        String filename = "FindFile.java";
        FindFile findFile = new FindFile(path, filename, FileType.CODE);
        String expectedFilePath = path + "/" + filename;
        assertEquals(expectedFilePath, findFile.toString());
    }
}

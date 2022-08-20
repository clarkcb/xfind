package javafind;

import org.junit.Test;

import java.io.File;

import static org.junit.Assert.assertEquals;

public class FileResultTest {

    public FileResultTest() {}

    @Test
    public final void test_fileresult_abs_path() {
        String path = "~/src/xfind/java/javafind/src/main/java/javafind";
        String filename = "FileResult.java";
        File file = new File(path + "/" + filename);
        FileResult fileResult = new FileResult(file, FileType.CODE);
        String expectedFilePath = path + "/" + filename;
        assertEquals(expectedFilePath, fileResult.toString());
    }

    @Test
    public final void test_fileresult_rel_path1() {
        String path = ".";
        String filename = "FileResult.java";
        File file = new File(path + "/" + filename);
        FileResult fileResult = new FileResult(file, FileType.CODE);
        String expectedFilePath = path + "/" + filename;
        assertEquals(expectedFilePath, fileResult.toString());
    }

    @Test
    public final void test_fileresult_rel_path2() {
        String path = "..";
        String filename = "FileResult.java";
        File file = new File(path + "/" + filename);
        FileResult fileResult = new FileResult(file, FileType.CODE);
        String expectedFilePath = path + "/" + filename;
        assertEquals(expectedFilePath, fileResult.toString());
    }
}

package javafind;

import org.junit.Test;

import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

public class FileResultTest {

    public FileResultTest() {}

    @Test
    public final void test_fileresult_abs_path() {
        String dir = "~/src/xfind/java/javafind/src/main/java/javafind";
        String filename = "FileResult.java";
        Path path = Paths.get(dir + "/" + filename);
        FileResult fileResult = new FileResult(path, FileType.CODE);
        String expectedFilePath = dir + "/" + filename;
        assertEquals(expectedFilePath, fileResult.toString());
    }

    @Test
    public final void test_fileresult_rel_path1() {
        String dir = ".";
        String filename = "FileResult.java";
        Path path = Paths.get(dir + "/" + filename);
        FileResult fileResult = new FileResult(path, FileType.CODE);
        String expectedFilePath = dir + "/" + filename;
        assertEquals(expectedFilePath, fileResult.toString());
    }

    @Test
    public final void test_fileresult_rel_path2() {
        String dir = "..";
        String filename = "FileResult.java";
        Path path = Paths.get(dir + "/" + filename);
        FileResult fileResult = new FileResult(path, FileType.CODE);
        String expectedFilePath = dir + "/" + filename;
        assertEquals(expectedFilePath, fileResult.toString());
    }
}

package javafind;

import org.junit.Test;

import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

public class FileResultTest {

    public FileResultTest() {}

    @Test
    public final void testFileResultAbsPath() {
        var dir = "~/src/xfind/java/javafind/src/main/java/javafind";
        var fileName = "FileResult.java";
        var path = Paths.get(dir + "/" + fileName);
        var fileResult = new FileResult(path, FileType.CODE);
        var expectedFilePath = dir + "/" + fileName;
        assertEquals(expectedFilePath, fileResult.toString());
    }

    @Test
    public final void testFileResultRelPath1() {
        var dir = ".";
        var fileName = "FileResult.java";
        var path = Paths.get(dir + "/" + fileName);
        var fileResult = new FileResult(path, FileType.CODE);
        var expectedFilePath = dir + "/" + fileName;
        assertEquals(expectedFilePath, fileResult.toString());
    }

    @Test
    public final void testFileResultRelPath2() {
        var dir = "..";
        var fileName = "FileResult.java";
        var path = Paths.get(dir + "/" + fileName);
        var fileResult = new FileResult(path, FileType.CODE);
        var expectedFilePath = dir + "/" + fileName;
        assertEquals(expectedFilePath, fileResult.toString());
    }
}

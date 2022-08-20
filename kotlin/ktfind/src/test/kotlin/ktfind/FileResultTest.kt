package ktfind

import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

class FileResultTest {

    @Test
    fun test_fileresult_abs_path() {
        val path = "/Users/cary/src/xfind/kotlin/ktfind/src/main/kotlin/ktfind/FileResult.kt"
        val fileResult = FileResult(File(path), FileType.CODE)
        assertEquals(path, fileResult.toString())
    }

    @Test
    fun test_fileresult_tilde_path() {
        val path = "~/src/xfind/kotlin/ktfind/src/main/kotlin/ktfind/FileResult.kt"
        val fileResult = FileResult(File(path), FileType.CODE)
        assertEquals(path, fileResult.toString())
    }

    @Test
    fun test_fileresult_rel_path1() {
        val path = "./FileResult.kt"
        val fileResult = FileResult(File(path), FileType.CODE)
        assertEquals(path, fileResult.toString())
    }

    @Test
    fun test_fileresult_rel_path2() {
        val path = "../FileResult.kt"
        val fileResult = FileResult(File(path), FileType.CODE)
        assertEquals(path, fileResult.toString())
    }
}

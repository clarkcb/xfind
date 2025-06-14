package ktfind

import java.nio.file.Paths
import kotlin.test.Test
import kotlin.test.assertEquals

class FileResultTest {

    @Test
    fun testFileResultAbsPath() {
        val path = "/Users/cary/src/xfind/kotlin/ktfind/src/main/kotlin/ktfind/FileResult.kt"
        val fileResult = FileResult(Paths.get(path), FileType.CODE)
        assertEquals(path, fileResult.toString())
    }

    @Test
    fun testFileResultTildePath() {
        val path = "~/src/xfind/kotlin/ktfind/src/main/kotlin/ktfind/FileResult.kt"
        val fileResult = FileResult(Paths.get(path), FileType.CODE)
        assertEquals(path, fileResult.toString())
    }

    @Test
    fun testFileResultRelPath1() {
        val path = "./FileResult.kt"
        val fileResult = FileResult(Paths.get(path), FileType.CODE)
        assertEquals(path, fileResult.toString())
    }

    @Test
    fun testFileResultRelPath2() {
        val path = "../FileResult.kt"
        val fileResult = FileResult(Paths.get(path), FileType.CODE)
        assertEquals(path, fileResult.toString())
    }
}

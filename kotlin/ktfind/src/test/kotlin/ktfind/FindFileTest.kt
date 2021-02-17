package ktfind

import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

class FindFileTest {

    @Test
    fun test_findfile_abs_path() {
        val path = "/Users/cary/src/xfind/kotlin/ktfind/src/main/kotlin/ktfind/FindFile.kt"
        val findFile = FindFile(File(path), FileType.CODE)
        assertEquals(path, findFile.toString())
    }

    @Test
    fun test_findfile_tilde_path() {
        val path = "~/src/xfind/kotlin/ktfind/src/main/kotlin/ktfind/FindFile.kt"
        val findFile = FindFile(File(path), FileType.CODE)
        assertEquals(path, findFile.toString())
    }

    @Test
    fun test_findfile_rel_path1() {
        val path = "./FindFile.kt"
        val findFile = FindFile(File(path), FileType.CODE)
        assertEquals(path, findFile.toString())
    }

    @Test
    fun test_findfile_rel_path2() {
        val path = "../FindFile.kt"
        val findFile = FindFile(File(path), FileType.CODE)
        assertEquals(path, findFile.toString())
    }
}

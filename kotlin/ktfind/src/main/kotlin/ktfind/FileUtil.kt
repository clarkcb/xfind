package ktfind

import java.io.File
import java.nio.file.Path

/**
 * @author cary on 7/24/16.
 */
object FileUtil {
    private val dotDirs: Set<String> = setOf(".", "..", "./", "../")
    //private val DEFAULT_ENCODING = "UTF-8"

    fun isDotDir(p: Path): Boolean {
        return isDotDir(p.toString())
    }

    fun isDotDir(f: String): Boolean {
        return dotDirs.contains(f)
    }

    fun isHidden(p: Path): Boolean {
        p.forEach { elem -> if (isHidden(elem.toString())) return true }
        return false
    }

    fun isHidden(f: String): Boolean {
        return f.isNotEmpty() && f[0] == '.' && !isDotDir(f)
    }

    fun splitPath(p: Path): List<String> {
        val elemList = mutableListOf<String>()
        p.forEach { elem -> elemList.add(elem.toString()) }
        return elemList
    }
}

package ktfind

import java.io.File

/**
 * @author cary on 7/24/16.
 */
object FileUtil {
    private val dotDirs: Set<String> = setOf(".", "..", "./", "../")
    //private val DEFAULT_ENCODING = "UTF-8"

    fun isDotDir(f: String): Boolean {
        return dotDirs.contains(f)
    }

    fun isHidden(f: String): Boolean {
        return f.isNotEmpty() && f[0] == '.' && !isDotDir(f)
    }

    fun sepCount(f: String): Long {
        return f.chars().filter { c -> c.toChar() == File.separatorChar }.count()
    }
}

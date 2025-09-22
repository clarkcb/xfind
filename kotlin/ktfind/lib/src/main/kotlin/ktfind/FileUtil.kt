package ktfind

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

/**
 * @author cary on 7/24/16.
 */
object FileUtil {
    private const val DOT = "."
    private const val DOT_DOT = ".."
    private val dotDirs: Set<String> = setOf(DOT, DOT_DOT, "./", "../")
    //private val DEFAULT_ENCODING = "UTF-8"
    private const val TILDE = "~"

    fun expandPath(path: Path): Path {
        val pathString = path.toString()
        if (pathString.startsWith(TILDE)) {
            val userPath = Paths.get(System.getProperty("user.home"))
            if (pathString == TILDE || pathString == TILDE + File.separator) {
                return userPath
            } else if (pathString.startsWith(TILDE + File.separator)) {
                return Paths.get(userPath.toString(), pathString.substring(2))
            }
            // Another user's home directory
            val homePath = userPath.parent
            return Paths.get(homePath.toString(), pathString.substring(1))
        }

        // Return the path as is if it doesn't contain a tilde.
        return path
    }

    fun isDotDir(p: Path): Boolean {
        return isDotDir(p.toString())
    }

    fun isDotDir(f: String): Boolean {
        return dotDirs.contains(f)
    }

    fun isHiddenName(f: String): Boolean {
        return f.isNotEmpty() && f[0] == '.' && !isDotDir(f)
    }

    fun isHiddenPath(p: Path): Boolean {
        p.forEach { elem -> if (isHiddenName(elem.toString())) return true }
        return false
    }

    fun splitPath(p: Path): List<String> {
        val elemList = mutableListOf<String>()
        p.forEach { elem -> elemList.add(elem.toString()) }
        return elemList
    }
}

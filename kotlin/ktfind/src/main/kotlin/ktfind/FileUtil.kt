package ktfind

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

/**
 * @author cary on 7/24/16.
 */
object FileUtil {
    private val dotDirs: Set<String> = setOf(".", "..", "./", "../")
    //private val DEFAULT_ENCODING = "UTF-8"

    fun expandPath(path: Path): Path {
        var pathString = path.toString()
        if (pathString.startsWith("~")) {
            var userPath = Paths.get(System.getProperty("user.home"))
            if (pathString == "~" || pathString == "~" + File.separator) {
                return userPath
            } else if (pathString.startsWith("~" + File.separator)) {
                return Paths.get(userPath.toString(), pathString.substring(2))
            }
            // Another user's home directory
            val homePath = userPath.parent
            var sepIndex = pathString.indexOf(File.separator)
            if (sepIndex == -1) {
                var userName = pathString.substring(1)
                return Paths.get(homePath.toString(), userName)
            }
            var userName = pathString.substring(1, sepIndex)
            userPath = Paths.get(homePath.toString(), userName)

            return Paths.get(userPath.toString(), pathString.substring(sepIndex + 1))
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

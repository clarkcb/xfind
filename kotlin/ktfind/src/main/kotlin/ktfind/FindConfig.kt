package ktfind

import java.nio.file.Path
import java.nio.file.Paths

/**
 * @author cary on 7/24/16.
 */
object FindConfig {
    val XFIND_PATH: Path =
        if (System.getenv().containsKey("XFIND_PATH"))
            Paths.get(System.getenv()["XFIND_PATH"]!!)
        else
            Paths.get(System.getProperty("user.home"), "src", "xfind")
    val XFIND_DB: Path = Paths.get(XFIND_PATH.toString(), "shared", "xfind.db")
}

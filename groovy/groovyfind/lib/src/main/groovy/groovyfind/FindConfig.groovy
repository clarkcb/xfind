package groovyfind

import groovy.transform.CompileStatic

import java.nio.file.Path
import java.nio.file.Paths

@CompileStatic
class FindConfig {
    public static final Path XFIND_PATH = getXfindPath()
    public static final Path XFIND_DB = Paths.get(XFIND_PATH.toString(), "shared", "xfind.db")

    private static Path getXfindPath() {
        if (System.getenv().containsKey("XFIND_PATH")) {
            return Paths.get(System.getenv("XFIND_PATH"));
        } else {
            return Paths.get(System.getProperty("user.home"), "src", "xfind");
        }
    }
}

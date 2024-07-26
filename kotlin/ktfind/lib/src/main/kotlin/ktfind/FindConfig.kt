package ktfind

/**
 * @author cary on 7/24/16.
 */
object FindConfig {
    private val HOME: String = System.getProperty("user.home")
    val XFINDPATH: String = System.getenv().getOrDefault("XFIND_PATH", "$HOME/src/xfind")
    val XFINDDB: String = "$XFINDPATH/shared/xfind.db"
}

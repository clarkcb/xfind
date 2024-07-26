package javafind;

public class FindConfig {
    private static final String HOME = System.getProperty("user.home");
    public static final String XFINDPATH = System.getenv().getOrDefault("XFIND_PATH", HOME + "/src/xfind");
    public static final String XFINDDB = XFINDPATH + "/shared/xfind.db";
}

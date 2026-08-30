package javafind;

import java.nio.file.Paths;

public class FindConfig {
    public static final String DEFAULT_FILE_TYPES_PATH = "/filetypes.json";
    public static final String DEFAULT_FIND_OPTIONS_PATH = "/findoptions.json";
    public static final String DEFAULT_XFIND_CONFIG_DIR =
            Paths.get(System.getProperty("user.home"), ".config", "xfind").toString();

    private final String fileTypesPath;
    private final String findOptionsPath;
    private final String defaultFindSettingsPath;

    public FindConfig() {
        this.fileTypesPath = DEFAULT_FILE_TYPES_PATH;
        this.findOptionsPath = DEFAULT_FIND_OPTIONS_PATH;
        var xFindConfigDir = getXFindConfigDir();
        this.defaultFindSettingsPath = Paths.get(xFindConfigDir, "settings.json").toString();
    }

    public String getFileTypesPath() {
        return fileTypesPath;
    }

    public String getFindOptionsPath() {
        return findOptionsPath;
    }

    public String getXFindConfigDir() {
        var xfindConfigDir = System.getenv("XFIND_CONFIG_DIR");
        if (xfindConfigDir == null || xfindConfigDir.isEmpty()) {
            xfindConfigDir = DEFAULT_XFIND_CONFIG_DIR;
        }
        return xfindConfigDir;
    }

    public String getDefaultFindSettingsPath() {
        return defaultFindSettingsPath;
    }
}

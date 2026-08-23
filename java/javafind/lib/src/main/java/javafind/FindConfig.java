package javafind;

import java.nio.file.Paths;

public class FindConfig {

    final private String fileTypesPath;
    final private String findOptionsPath;
    final private String defaultFindSettingsPath;

    public FindConfig() {
        this.fileTypesPath = "/filetypes.json";
        this.findOptionsPath = "/findoptions.json";
        var home = System.getProperty("user.home");
        this.defaultFindSettingsPath = Paths.get(home, ".config", "xfind", "settings.json").toString();
    }

    public String getFileTypesPath() {
        return fileTypesPath;
    }

    public String getFindOptionsPath() {
        return findOptionsPath;
    }

    public String getDefaultFindSettingsPath() {
        return defaultFindSettingsPath;
    }
}

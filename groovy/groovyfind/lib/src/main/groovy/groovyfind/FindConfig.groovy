package groovyfind

import groovy.transform.CompileStatic

import java.nio.file.Paths

@CompileStatic
class FindConfig {

    final String defaultXFindConfigDir = Paths.get(System.getProperty("user.home"), ".config", "xfind").toString()
    final String fileTypesPath
    final String findOptionsPath
    final String defaultFindSettingsPath

    FindConfig() {
        this.fileTypesPath = "/filetypes.json"
        this.findOptionsPath = "/findoptions.json"
        var xfindConfigDir = System.getenv("XFIND_CONFIG_DIR")
        if (xfindConfigDir == null || xfindConfigDir.isEmpty()) {
            xfindConfigDir = defaultXFindConfigDir
        }
        this.defaultFindSettingsPath = Paths.get(xfindConfigDir, "settings.json").toString()
    }
}

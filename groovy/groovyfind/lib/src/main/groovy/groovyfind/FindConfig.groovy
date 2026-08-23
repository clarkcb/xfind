package groovyfind

import groovy.transform.CompileStatic

import java.nio.file.Paths

@CompileStatic
class FindConfig {

    final String fileTypesPath
    final String findOptionsPath
    final String defaultFindSettingsPath

    FindConfig() {
        this.fileTypesPath = "/filetypes.json"
        this.findOptionsPath = "/findoptions.json"
        String home = System.getProperty("user.home")
        this.defaultFindSettingsPath = Paths.get(home, ".config", "xfind", "settings.json").toString()
    }
}

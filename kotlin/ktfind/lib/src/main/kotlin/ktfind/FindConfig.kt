package ktfind

import java.nio.file.Paths

class FindConfig {
    val fileTypesPath: String
    val findOptionsPath: String
    val defaultFindSettingsPath: String

    init {
        fileTypesPath = "/filetypes.json"
        findOptionsPath = "/findoptions.json"
        val defaultSettingsPath = Paths.get(System.getProperty("user.home"), ".config", "xfind", "settings.json")
        defaultFindSettingsPath = defaultSettingsPath.toString()
    }
}

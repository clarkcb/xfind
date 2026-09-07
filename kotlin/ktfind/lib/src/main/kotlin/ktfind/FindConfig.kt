package ktfind

import java.nio.file.Paths

open class FindConfig {
    val defaultXFindConfigDir = Paths.get(System.getProperty("user.home"), ".config", "xfind").toString()
    val fileTypesPath: String
    val findOptionsPath: String
    val defaultFindSettingsPath: String

    init {
        fileTypesPath = "/filetypes.json"
        findOptionsPath = "/findoptions.json"
        val xFindConfigDir = System.getenv("XFIND_CONFIG_DIR") ?: defaultXFindConfigDir
        defaultFindSettingsPath = Paths.get(xFindConfigDir, "settings.json").toString()
    }
}

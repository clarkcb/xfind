package scalafind

import java.nio.file.Paths

class FindConfig {
  val defaultXFindConfigDir: String = Paths.get(System.getProperty("user.home"), ".config", "xfind").toString
  val fileTypesPath: String = "/filetypes.json"
  val findOptionsPath: String = "/findoptions.json"
  val xFindConfigDir: String = {
    if (System.getenv("XFIND_CONFIG_DIR") != null) {
      System.getenv("XFIND_CONFIG_DIR")
    } else {
      defaultXFindConfigDir
    }
  }
  val defaultFindSettingsPath: String = Paths.get(xFindConfigDir, "settings.json").toString
}

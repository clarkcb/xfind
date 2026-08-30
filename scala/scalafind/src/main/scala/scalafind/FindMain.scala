package scalafind

object FindMain {

  def main(args: Array[String]): Unit = {
    var colorize = true
    var findOptions: Option[FindOptions] = None

    try {
      val config = new FindConfig()
      findOptions = Some(new FindOptions(config))
      val settings = findOptions.get.settingsFromArgs(args)
      colorize = settings.colorize

      if (settings.debug) {
        Common.log("settings: " + settings)
      }

      if (settings.printUsage) {
        Common.log("")
        findOptions.foreach(_.usage(0))
      }

      val finder = new Finder(config, settings)
      val fileResults = finder.find()
      val formatter = new FileResultFormatter(settings)

      if (settings.printDirs) { Finder.printMatchingDirs(fileResults, formatter) }
      if (settings.printFiles) { Finder.printMatchingFiles(fileResults, formatter) }

    } catch {
      case e: FindException =>
        Common.log("")
        Common.logError(e.getMessage + "\n", colorize)
        findOptions.foreach(_.usage(1))
    }
  }
}

package scalafind

object FindMain {

  def main(args: Array[String]): Unit = {
    try {
      val settings = FindOptions.settingsFromArgs(args)

      if (settings.debug) {
        Common.log("settings: " + settings)
      }

      if (settings.printUsage) {
        Common.log("")
        FindOptions.usage(0)
      }

      val finder = new Finder(settings)
      val fileResults = finder.find()
      val formatter = new FileResultFormatter(settings)

      if (settings.printDirs) { finder.printMatchingDirs(fileResults, formatter) }
      if (settings.printFiles) { finder.printMatchingFiles(fileResults, formatter) }

    } catch {
      case e: FindException =>
        Common.log("")
        Common.logError(e.getMessage + "\n")
        FindOptions.usage(1)
    }
  }
}

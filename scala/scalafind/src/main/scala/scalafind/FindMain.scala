package scalafind

import java.nio.file.Path

object FindMain {

  private def getMatchingDirs(fileResults: Seq[FileResult]): Seq[Path] = {
    fileResults
      .map(fr => FileUtil.pathOrCurrent(fr.path))
      .distinct
      .sortWith(_.toString < _.toString)
  }

  private def printMatchingDirs(fileResults: Seq[FileResult], formatter: FileResultFormatter): Unit = {
    val dirs = getMatchingDirs(fileResults)
    if (dirs.nonEmpty) {
      Common.log("\nMatching directories (%d):".format(dirs.length))
      dirs.foreach(d => Common.log(formatter.formatDirPath(d)))
    } else {
      Common.log("\nMatching directories: 0")
    }
  }

  private def printMatchingFiles(fileResults: Seq[FileResult], formatter: FileResultFormatter): Unit = {
    if (fileResults.nonEmpty) {
      Common.log("\nMatching files (%d):".format(fileResults.length))
      fileResults.foreach(fr => Common.log(formatter.formatFileResult(fr)))
    } else {
      Common.log("\nMatching files: 0")
    }
  }

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

      if (settings.printDirs) { printMatchingDirs(fileResults, formatter) }
      if (settings.printFiles) { printMatchingFiles(fileResults, formatter) }

    } catch {
      case e: FindException =>
        Common.log("")
        Common.logError(e.getMessage + "\n")
        FindOptions.usage(1)
    }
  }
}

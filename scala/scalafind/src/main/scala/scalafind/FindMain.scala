package scalafind

import java.io.File
import java.nio.file.Path

object FindMain {

  private def getMatchingDirs(fileResults: Seq[FileResult]): Seq[Path] = {
    fileResults
      .map(fr => FileUtil.pathOrCurrent(fr.path))
      .distinct
      .sortWith(_.toString < _.toString)
  }

  private def getMatchingFiles(fileResults: Seq[FileResult]): Seq[String] = {
    fileResults
      .map(_.toString)
      .distinct
  }

  private def printMatchingDirs(fileResults: Seq[FileResult]): Unit = {
    val dirs = getMatchingDirs(fileResults)
    if (dirs.nonEmpty) {
      Common.log("\nMatching directories (%d):".format(dirs.length))
      dirs.foreach(f => Common.log(f.toString))
    } else {
      Common.log("\nMatching directories: 0")
    }
  }

  private def printMatchingFiles(fileResults: Seq[FileResult]): Unit = {
    val files = getMatchingFiles(fileResults)
    if (files.nonEmpty) {
      Common.log("\nMatching files (%d):".format(files.length))
      files.foreach(f => Common.log(f.toString))
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

      if (settings.printDirs) { printMatchingDirs(fileResults) }
      if (settings.printFiles) { printMatchingFiles(fileResults) }

    } catch {
      case e: FindException =>
        Common.log("")
        Common.logError(e.getMessage + "\n")
        FindOptions.usage(1)
    }
  }
}

package scalafind

import java.io.File

object FindMain {

  private def cmpFindResults(r1: FindResult, r2: FindResult): Boolean = {
    val (path1, fileName1) = r1.file match {
      case Some(file1) =>
        (FileUtil.pathOrCurrent(file1.file.getParentFile).getPath, file1.file.getName.toLowerCase)
      case None => ("", "")
    }
    val (path2, fileName2) = r2.file match {
      case Some(file2) =>
        (FileUtil.pathOrCurrent(file2.file.getParentFile).getPath, file2.file.getName.toLowerCase)
      case None => ("", "")
    }
    if (path1 == path2) {
      if (fileName1 == fileName2) {
        if (r1.lineNum == r2.lineNum) {
          r1.matchStartIndex < r2.matchStartIndex
        } else {
          r1.lineNum < r2.lineNum
        }
      } else {
        fileName1 < fileName2
      }
    } else {
      path1 < path2
    }
  }

  def printFindResults(results: Seq[FindResult], settings: FindSettings): Unit = {
    // TODO: add includePattern setting in formatted output
    val formatter = new FindResultFormatter(settings)
    results.sortWith(cmpFindResults).foreach(r => Common.log(formatter.format(r)))
  }

  def getMatchingDirs(results: Seq[FindResult]): Seq[File] = {
    results
      .filter(_.file.isDefined)
      .map(r => FileUtil.pathOrCurrent(r.file.get.file.getParentFile))
      .distinct
      .toVector
  }

  def getMatchingFiles(results: Seq[FindResult]): Seq[File] = {
    results
      .filter(_.file.isDefined)
      .map(_.file.get.file)
      .distinct
      .toVector
  }

  def getMatchingLines(results: Seq[FindResult], settings: FindSettings): Seq[String] = {
    val allLines = results.flatMap(r => r.line).map(_.trim)
    if (settings.uniqueLines) {
      allLines.distinct.sortWith(_.toUpperCase < _.toUpperCase)
    } else {
      allLines.sortWith(_.toUpperCase < _.toUpperCase)
    }
  }

  def printMatchingDirs(results: Seq[FindResult]): Unit = {
    val dirs = getMatchingDirs(results)
    Common.log("\nDirectories with matches (%d):".format(dirs.length))
    dirs.foreach(f => Common.log(f.toString))
  }

  def printMatchingFiles(results: Seq[FindResult]): Unit = {
    val files = getMatchingFiles(results)
    Common.log("\nFiles with matches (%d):".format(files.length))
    files.foreach(f => Common.log(f.toString))
  }

  def printMatchingLines(results: Seq[FindResult], settings: FindSettings): Unit = {
    val lines = getMatchingLines(results, settings)
    val hdr =
      if (settings.uniqueLines) {
        "\nUnique lines with matches (%d):"
      } else {
        "\nLines with matches (%d):"
      }
    Common.log(hdr.format(lines.length))
    lines.foreach(Common.log)
  }

  def main(args: Array[String]) {
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
      val results = finder.find()

      if (settings.printResults) {
        Common.log("\nFind results (%d):".format(results.length))
        printFindResults(results, settings)
      }
      if (settings.listDirs) { printMatchingDirs(results) }
      if (settings.listFiles) { printMatchingFiles(results) }
      if (settings.listLines) { printMatchingLines(results, settings) }

    } catch {
      case e: FindException =>
        Common.log("")
        Common.logError(e.getMessage + "\n")
        FindOptions.usage(1)
    }
  }
}

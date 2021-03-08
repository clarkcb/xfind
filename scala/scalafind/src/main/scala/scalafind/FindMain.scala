package scalafind

import java.io.File

object FindMain {

  def getMatchingDirs(findfiles: Seq[FindFile]): Seq[File] = {
    findfiles
      .map(f => FileUtil.pathOrCurrent(f.file))
      .distinct
      .sortWith(_.toString < _.toString)
      .toVector
  }

  private def cmpFiles(f1: File, f2: File): Boolean = {
    val (path1, fileName1) = Option(f1) match {
      case Some(file1) =>
        (FileUtil.pathOrCurrent(file1).toString, file1.getName.toLowerCase)
      case None => ("", "")
    }
    val (path2, fileName2) = Option(f2) match {
      case Some(file2) =>
        (FileUtil.pathOrCurrent(file2).toString, file2.getName.toLowerCase)
      case None => ("", "")
    }
    if (path1 == path2) {
      fileName1 < fileName2
    } else {
      path1 < path2
    }
  }

  def getMatchingFiles(findfiles: Seq[FindFile]): Seq[File] = {
    findfiles
      .map(_.file)
      .distinct
      .toVector
      .sortWith(cmpFiles)
  }

  def printMatchingDirs(findfiles: Seq[FindFile]): Unit = {
    val dirs = getMatchingDirs(findfiles)
    if (dirs.nonEmpty) {
      Common.log("\nMatching directories (%d):".format(dirs.length))
      dirs.foreach(f => Common.log(f.toString))
    } else {
      Common.log("\nMatching directories: 0")
    }
  }

  def printMatchingFiles(findfiles: Seq[FindFile]): Unit = {
    val files = getMatchingFiles(findfiles)
    if (files.nonEmpty) {
      Common.log("\nMatching files (%d):".format(files.length))
      files.foreach(f => Common.log(f.toString))
    } else {
      Common.log("\nMatching files: 0")
    }
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
      val findFiles = finder.find()

      if (settings.listDirs) { printMatchingDirs(findFiles) }
      if (settings.listFiles) { printMatchingFiles(findFiles) }

    } catch {
      case e: FindException =>
        Common.log("")
        Common.logError(e.getMessage + "\n")
        FindOptions.usage(1)
    }
  }
}

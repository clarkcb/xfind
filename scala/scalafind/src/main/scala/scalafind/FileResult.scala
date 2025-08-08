package scalafind

import scalafind.FileResult.{CONTAINER_SEPARATOR, comparePaths}

import java.nio.file.{Path, Paths}
import java.nio.file.attribute.FileTime

object FileResult {
  val CONTAINER_SEPARATOR = "!"

  def comparePaths(path1: Path, path2: Path, sortCaseInsensitive: Boolean): Int = {
    def pathToName(p: Path, sortCaseInsensitive: Boolean): String = {
      (Option(p), sortCaseInsensitive) match {
        case (None, _) => ""
        case (Some(p), true) => p.toString.toLowerCase
        case (Some(p), false) => p.toString
      }
    }
    val p1 = pathToName(path1, sortCaseInsensitive)
    val p2 = pathToName(path2, sortCaseInsensitive)
    p1.compareTo(p2)
  }
}

class FileResult(val containers: List[Path],
                 val path: Path,
                 val fileType: FileType.Value,
                 val fileSize: Long,
                 val lastMod: Option[FileTime] = None) {

  def this(path: Path, fileType: FileType.Value) = {
    this(List.empty[Path], path, fileType, 0L, None)
  }

  def this(path: Path, fileType: FileType.Value, fileSize: Long, lastMod: Option[FileTime]) = {
    this(List.empty[Path], path, fileType, fileSize, lastMod)
  }

  def compareByPath(other: FileResult, sortCaseInsensitive: Boolean): Int = {
    val pComp = comparePaths(this.path.getParent, other.path.getParent, sortCaseInsensitive)
    if (pComp == 0) {
      val fComp = comparePaths(this.path.getFileName, other.path.getFileName, sortCaseInsensitive)
      fComp
    } else {
      pComp
    }
  }

  def beforeByPath(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareByPath(other, sortCaseInsensitive)
    cmp < 0
  }

  def compareByName(other: FileResult, sortCaseInsensitive: Boolean): Int = {
    val fComp = comparePaths(this.path.getFileName, other.path.getFileName, sortCaseInsensitive)
    if (fComp == 0) {
      val pComp = comparePaths(this.path.getParent, other.path.getParent, sortCaseInsensitive)
      pComp
    } else {
      fComp
    }
  }

  def beforeByName(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareByName(other, sortCaseInsensitive)
    cmp < 0
  }

  def compareBySize(other: FileResult, sortCaseInsensitive: Boolean): Int = {
    if (this.fileSize == other.fileSize) {
      compareByPath(other, sortCaseInsensitive)
    } else {
      if (this.fileSize < other.fileSize) -1 else 1
    }
  }

  def beforeBySize(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareBySize(other, sortCaseInsensitive)
    cmp < 0
  }

  def compareByType(other: FileResult, sortCaseInsensitive: Boolean): Int = {
    if (this.fileType.equals(other.fileType)) {
      compareByPath(other, sortCaseInsensitive)
    } else {
      if (this.fileType < other.fileType) -1 else 1
    }
  }

  def beforeByType(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareByType(other, sortCaseInsensitive)
    cmp < 0
  }

  def compareByLastMod(other: FileResult, sortCaseInsensitive: Boolean): Int = {
    (this.lastMod, other.lastMod) match {
      case (Some(lastMod1), Some(lastMod2)) =>
        if (lastMod1 == lastMod2) {
          compareByPath(other, sortCaseInsensitive)
        } else {
          lastMod1.compareTo(lastMod2)
        }
      case (_, _) => 0
    }
  }

  def beforeByLastMod(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareByLastMod(other, sortCaseInsensitive)
    cmp < 0
  }

  override def toString : String = {
    val sb = new StringBuilder
    if (containers.nonEmpty) {
      sb.append(containers.mkString(CONTAINER_SEPARATOR)).append(CONTAINER_SEPARATOR)
    }
    sb.append(path.toString)
    sb.toString()
  }
}

class FileResultFormatter(val settings: FindSettings) {

  def colorize(s: String, matchStartIndex: Int, matchEndIndex: Int): String = {
    val prefix = if (matchStartIndex > 0) {
      s.substring(0, matchStartIndex)
    } else {
      ""
    }
    val suffix = if (matchEndIndex < s.length) {
      s.substring(matchEndIndex)
    } else {
      ""
    }
    prefix +
      Color.GREEN.toString +
      s.substring(matchStartIndex, matchEndIndex) +
      Color.RESET.toString +
      suffix
  }

  private def formatDirPathWithColor(dirPath: Path): String = {
    var formattedDirPath = dirPath.toString
    settings.inDirPatterns.flatMap(p => p.findFirstMatchIn(formattedDirPath)).take(1).foreach { m =>
      formattedDirPath = colorize(formattedDirPath, m.start, m.end)
    }
    formattedDirPath
  }

  val formatDirPath: Path => String =
    if (settings.colorize && settings.inDirPatterns.nonEmpty) {
      formatDirPathWithColor
    } else {
      (dirPath: Path) => dirPath.toString
    }

  private def formatFileNameWithColor(fileName: String): String = {
    var formattedFileName = fileName
    settings.inFilePatterns.flatMap(p => p.findFirstMatchIn(formattedFileName)).take(1).foreach { m =>
      formattedFileName = colorize(formattedFileName, m.start, m.end)
    }
    if (settings.inExtensions.nonEmpty) {
      val idx = formattedFileName.lastIndexOf('.')
      if (idx > 0 && idx < formattedFileName.length - 1) {
        formattedFileName = colorize(formattedFileName, idx + 1, formattedFileName.length)
      }
    }
    formattedFileName
  }

  val formatFileName: String => String =
    if (settings.colorize && (settings.inExtensions.nonEmpty || settings.inFilePatterns.nonEmpty)) {
      formatFileNameWithColor
    } else {
      (fileName: String) => fileName
    }

  def formatPath(path: Path): String = {
    var parent = "."
    if (path.getParent != null) {
      parent = formatDirPath(path.getParent)
    }
    val fileName = formatFileName(path.getFileName.toString)
    Paths.get(parent, fileName).toString
  }

  def formatFileResult(result: FileResult): String = {
    formatPath(result.path)
  }
}

class FileResultSorter(val settings: FindSettings) {
  private def getFileResultComparator: (FileResult, FileResult) => Boolean = {
    if (settings.sortDescending) {
      settings.sortBy match {
        case SortBy.FileName => (fr1: FileResult, fr2: FileResult) => fr2.beforeByName(fr1, settings.sortCaseInsensitive)
        case SortBy.FileSize => (fr1: FileResult, fr2: FileResult) => fr2.beforeBySize(fr1, settings.sortCaseInsensitive)
        case SortBy.FileType => (fr1: FileResult, fr2: FileResult) => fr2.beforeByType(fr1, settings.sortCaseInsensitive)
        case SortBy.LastMod => (fr1: FileResult, fr2: FileResult) => fr2.beforeByLastMod(fr1, settings.sortCaseInsensitive)
        case _ => (fr1: FileResult, fr2: FileResult) => fr2.beforeByPath(fr1, settings.sortCaseInsensitive)
      }
    } else {
      settings.sortBy match {
        case SortBy.FileName => (fr1: FileResult, fr2: FileResult) => fr1.beforeByName(fr2, settings.sortCaseInsensitive)
        case SortBy.FileSize => (fr1: FileResult, fr2: FileResult) => fr1.beforeBySize(fr2, settings.sortCaseInsensitive)
        case SortBy.FileType => (fr1: FileResult, fr2: FileResult) => fr1.beforeByType(fr2, settings.sortCaseInsensitive)
        case SortBy.LastMod => (fr1: FileResult, fr2: FileResult) => fr1.beforeByLastMod(fr2, settings.sortCaseInsensitive)
        case _ => (fr1: FileResult, fr2: FileResult) => fr1.beforeByPath(fr2, settings.sortCaseInsensitive)
      }
    }
  }

  def sort(fileResults: Seq[FileResult]): Seq[FileResult] = {
    val fileResultComparator = getFileResultComparator
    fileResults.sortWith(fileResultComparator)
  }
}

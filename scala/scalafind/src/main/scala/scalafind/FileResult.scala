package scalafind

import java.nio.file.Path
import java.nio.file.attribute.FileTime

class FileResult(val containers: List[String],
                 val path: Path,
                 val fileType: FileType.Value,
                 val fileSize: Long,
                 val lastMod: Option[FileTime] = None) {

  val CONTAINER_SEPARATOR = "!"

  def this(path: Path, fileType: FileType.Value) = {
    this(List.empty[String], path, fileType, 0L, None)
  }

  def this(path: Path, fileType: FileType.Value, fileSize: Long, lastMod: Option[FileTime]) = {
    this(List.empty[String], path, fileType, fileSize, lastMod)
  }

  private def comparePaths(path1: Path, path2: Path, sortCaseInsensitive: Boolean): Int = {
    val p1 = (Option(path1), sortCaseInsensitive) match {
      case (None, _) => ""
      case (Some(p), true) => p.toString.toLowerCase
      case (Some(p), false) => p.toString
    }
    val p2 = (Option(path2), sortCaseInsensitive) match {
      case (None, _) => ""
      case (Some(p), true) => p.toString.toLowerCase
      case (Some(p), false) => p.toString
    }
    p1.compareTo(p2)
  }

  def compareByPath(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val pres = comparePaths(this.path.getParent, other.path.getParent, sortCaseInsensitive)
    if (pres == 0) {
      val fres = comparePaths(this.path.getFileName, other.path.getFileName, sortCaseInsensitive)
      fres < 0
    } else {
      pres < 0
    }
  }

  def compareByName(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val fres = comparePaths(this.path.getFileName, other.path.getFileName, sortCaseInsensitive)
    if (fres == 0) {
      val pres = comparePaths(this.path.getParent, other.path.getParent, sortCaseInsensitive)
      pres < 0
    } else {
      fres < 0
    }
  }

  def compareBySize(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    if (this.fileSize == other.fileSize) {
      compareByPath(other, sortCaseInsensitive)
    } else {
      this.fileSize < other.fileSize
    }
  }

  def compareByType(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    if (this.fileType.equals(other.fileType)) {
      compareByPath(other, sortCaseInsensitive)
    } else {
      this.fileType < other.fileType
    }
  }

  def compareByLastMod(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    (this.lastMod, other.lastMod) match {
      case (Some(lastMod1), Some(lastMod2)) =>
        if (lastMod1 == lastMod2) {
          compareByPath(other, sortCaseInsensitive)
        } else {
          lastMod1.compareTo(lastMod2) < 0
        }
      case (_, _) => false
    }
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

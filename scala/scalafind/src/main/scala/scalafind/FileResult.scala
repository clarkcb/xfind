package scalafind

import scalafind.FileResult.{CONTAINER_SEPARATOR, comparePaths}

import java.nio.file.Path
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

  def compareByPath(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val pComp = comparePaths(this.path.getParent, other.path.getParent, sortCaseInsensitive)
    if (pComp == 0) {
      val fComp = comparePaths(this.path.getFileName, other.path.getFileName, sortCaseInsensitive)
      fComp < 0
    } else {
      pComp < 0
    }
  }

  def compareByName(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val fComp = comparePaths(this.path.getFileName, other.path.getFileName, sortCaseInsensitive)
    if (fComp == 0) {
      val pComp = comparePaths(this.path.getParent, other.path.getParent, sortCaseInsensitive)
      pComp < 0
    } else {
      fComp < 0
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

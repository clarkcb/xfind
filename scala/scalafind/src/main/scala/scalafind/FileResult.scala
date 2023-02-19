package scalafind

import java.io.File
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes

class FileResult(val containers: List[String],
                 val path: Path,
                 val fileType: FileType.Value,
                 val stat: Option[BasicFileAttributes] = None) {

  val CONTAINER_SEPARATOR = "!"

  def this(path: Path, fileType: FileType.Value) = {
    this(List.empty[String], path, fileType, None)
  }

  def this(path: Path, fileType: FileType.Value, stat: Option[BasicFileAttributes]) = {
    this(List.empty[String], path, fileType, stat)
  }

  private def comparePaths(path1: Path, path2: Path, sortCaseInsensitive: Boolean): Int = {
    val p1 = (Option(path1), sortCaseInsensitive) match {
      case (None, _) => ""
      case (Some(p), true) => p.toString().toLowerCase()
      case (Some(p), false) => p.toString()
    }
    val p2 = (Option(path2), sortCaseInsensitive) match {
      case (None, _) => ""
      case (Some(p), true) => p.toString().toLowerCase()
      case (Some(p), false) => p.toString()
    }
    p1.compareTo(p2)
  }

  def compareByPath(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val pres = comparePaths(this.path.getParent(), other.path.getParent(), sortCaseInsensitive)
    if (pres == 0) {
      val fres = comparePaths(this.path.getFileName(), other.path.getFileName(), sortCaseInsensitive)
      fres < 0
    } else {
      pres < 0
    }
  }

  def compareByName(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    val fres = comparePaths(this.path.getFileName(), other.path.getFileName(), sortCaseInsensitive)
    if (fres == 0) {
      val pres = comparePaths(this.path.getParent(), other.path.getParent(), sortCaseInsensitive)
      pres < 0
    } else {
      fres < 0
    }
  }

  def compareBySize(other: FileResult, sortCaseInsensitive: Boolean): Boolean = {
    (this.stat, other.stat) match {
      case (Some(st1), Some(st2)) =>
        if (st1.size() == st2.size()) {
          compareByPath(other, sortCaseInsensitive)
        } else {
          st1.size() < st2.size()
        }
      case (_, _) => false
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
    (this.stat, other.stat) match {
      case (Some(st1), Some(st2)) =>
        if (st1.lastModifiedTime() == st2.lastModifiedTime()) {
          compareByPath(other, sortCaseInsensitive)
        } else {
          st1.lastModifiedTime().compareTo(st2.lastModifiedTime()) < 0
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

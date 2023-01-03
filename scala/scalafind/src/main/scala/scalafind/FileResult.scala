package scalafind

import java.io.File
import java.nio.file.Path

class FileResult(val containers: List[String], val path: Path, val fileType: FileType.Value) {

  val CONTAINER_SEPARATOR = "!"

  def this(path: Path, fileType: FileType.Value) = {
    this(List.empty[String], path, fileType)
  }

  def compareByPath(other: FileResult): Boolean = {
    if (this.path.getParent().equals(other.path.getParent())) {
      this.path.getFileName().compareTo(other.path.getFileName()) < 0
    } else {
      this.path.getParent().compareTo(other.path.getParent()) < 0
    }
  }

  def compareByName(other: FileResult): Boolean = {
    if (this.path.getFileName().equals(other.path.getFileName())) {
      this.path.getParent().compareTo(other.path.getParent()) < 0
    } else {
      this.path.getFileName().compareTo(other.path.getFileName()) < 0
    }
  }

  def compareByType(other: FileResult): Boolean = {
    if (this.fileType.equals(other.fileType)) {
      compareByPath(other)
    } else {
      this.fileType < other.fileType
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

package scalafind

import java.io.File
import java.nio.file.{Path, Paths}
import scala.io.{Codec, Source}

object FileUtil {
  val CURRENT_PATH = "."
  val PARENT_PATH = ".."
  val DEFAULT_ENCODING = "UTF-8"

  def expandPath(path: Path): Path = {
    if (path == null) throw new IllegalArgumentException("Path cannot be null")
    val tilde = "~"
    path match {
      case p if p.toString.startsWith(tilde) =>
        val pathString = path.toString
        val userPath: Path =
          if (pathString == tilde || pathString.startsWith(tilde + File.separator)) {
            // Current user's home directory
            Paths.get(System.getProperty("user.home"))
          } else {
            // Another user's home directory
            val homePath = Paths.get(System.getProperty("user.home")).getParent
            val sepIndex = pathString.indexOf(File.separator)
            val userName: String = if (sepIndex == -1) pathString.substring(1) else pathString.substring(1, sepIndex)
            Paths.get(homePath.toString, userName)
          }
        Paths.get(userPath.toString, pathString.substring(pathString.indexOf(File.separator) + 1))

      case _ => path
    }
  }

  def getExtension(f: FileResult): String = {
    getExtension(f.path.getFileName.toString)
  }

  def getExtension(p: Path): String = {
    getExtension(p.getFileName.toString)
  }

  def getExtension(name: String): String = {
    val lastIndex = name.lastIndexOf('.')
    if (lastIndex > 0 && lastIndex < name.length-1) {
      name.split('.').last
    } else {
      ""
    }
  }

  def getFileContents(f: File, enc: String=DEFAULT_ENCODING): String = {
    val bufferedSource = Source.fromFile(f, enc)
    val contents = bufferedSource.mkString
    bufferedSource.close
    contents
  }

  def getFileContents(f: File, codec: Codec): String = {
    val bufferedSource = Source.fromFile(f)(codec)
    val contents = bufferedSource.mkString
    bufferedSource.close
    contents
  }

  def isDotDir(name: String): Boolean = {
    Set(CURRENT_PATH, PARENT_PATH).contains(name)
  }

  def isHidden(name: String): Boolean = {
    val n = new File(name).getName
    n.length > 1 && n.startsWith(CURRENT_PATH) && !isDotDir(n)
  }

  def pathOrCurrent(p: Path): Path = {
    Option(p.getParent) match {
      case Some(dir) => dir
      case None => Paths.get(CURRENT_PATH)
    }
  }

  def splitPath(path: String): Iterable[String] = {
    path.split(File.separator).filterNot(_.isEmpty).filterNot(isDotDir)
  }

  def splitPath(path: Path): Iterable[String] = {
    Option(path) match {
      case Some(p) => splitPath(p.toString)
      case None => Iterable.empty
    }
  }

  def sepCount(path: String): Int = {
    path.count(_ == File.separatorChar)
  }
}

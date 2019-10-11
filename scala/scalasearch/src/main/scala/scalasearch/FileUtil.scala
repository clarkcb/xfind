package scalasearch

import java.io.File

import scala.io.Source

object FileUtil {
  def getExtension(f: SearchFile): String = {
    getExtension(f.fileName)
  }

  def getExtension(name: String): String = {
    val lastIndex = name.lastIndexOf('.')
    if (lastIndex > 0 && lastIndex < name.length-1) {
      name.split('.').last
    } else {
      ""
    }
  }

  def getFileContents(f: File, enc: String=DefaultSettings.textFileEncoding): String = {
    val bufferedSource = Source.fromFile(f, enc)
    val contents = bufferedSource.mkString
    bufferedSource.close
    contents
  }

  def isDotDir(name: String): Boolean = {
    Set(".", "..").contains(name)
  }

  def isHidden(name: String): Boolean = {
    val n = new File(name).getName
    n.length > 1 && n.startsWith(".") && !isDotDir(n)
  }

  def splitPath(path: String): Iterable[String] = {
    path.split(File.separator).filterNot(_.isEmpty).filterNot(isDotDir)
  }
}

package scalafind

import org.sqlite.SQLiteConfig
import scalafind.FileType.FileType

import java.io.IOException
import java.nio.file.Path
import scala.collection.mutable
import java.sql.{Connection, DriverManager, SQLException}
import scala.jdk.CollectionConverters.*

object FileType extends Enumeration {
  type FileType = Value
  val Unknown: FileType = Value("unknown")
  val Archive: FileType = Value("archive")
  val Audio: FileType   = Value("audio")
  val Binary: FileType  = Value("binary")
  val Code: FileType    = Value("code")
  val Font: FileType    = Value("font")
  val Image: FileType   = Value("image")
  val Text: FileType    = Value("text")
  val Video: FileType   = Value("video")
  val Xml: FileType     = Value("xml")

  def forName(fileTypeName: String): FileType = {
    try {
      FileType.withName(fileTypeName.trim.toLowerCase)
    } catch {
      case _: NoSuchElementException => Unknown
    }
  }
}

class FileTypes {

  private val fileTypes = List(
    FileType.Unknown, FileType.Archive, FileType.Audio, FileType.Binary, FileType.Code,
    FileType.Font, FileType.Image, FileType.Text, FileType.Video, FileType.Xml
  )

  private val SELECT_FILE_TYPE_ID_FOR_FILE_NAME = "SELECT file_type_id FROM file_name WHERE name = ?"
  private val SELECT_FILE_TYPE_ID_FOR_EXTENSION = "SELECT file_type_id FROM file_extension WHERE extension = ?"

  private var _conn: Option[Connection] = None
  private var _extTypeCache = Map[String, FileType]()
  private var _nameTypeCache = Map[String, FileType]()
  private var _nameTypeCacheLoaded = false

  private def getConnection: Option[Connection] = {
    Class.forName("org.sqlite.JDBC")
    _conn match {
      case Some(c) => _conn
      case None =>
        try {
          val config = SQLiteConfig()
          config.setReadOnly(true)
          _conn = Some(DriverManager.getConnection("jdbc:sqlite:" + FindConfig.XFIND_DB, config.toProperties))
          _conn
        } catch {
          case e: Exception =>
            Common.logError(e.getMessage)
            None
        }
    }
  }

  private def getFileTypesForQueryAndParams(query: String, params: List[String]): Map[String, FileType] = {
    val results = mutable.Map[String, FileType]()
    getConnection match
      case Some(conn) =>
        try {
          val stmt = conn.prepareStatement(query)
          params.zipWithIndex.foreach { case (param, idx) => stmt.setString(idx + 1, param) }
          val rs = stmt.executeQuery()
          while (rs.next()) {
            val key = rs.getString(1)
            val fileTypeId = rs.getInt(2) - 1
            results(key) = fileTypes(fileTypeId)
          }
          Map.empty[String, FileType] ++ results
        } catch {
          case e: SQLException =>
            Common.logError(e.getMessage)
            Map.empty[String, FileType] ++ results
        }
      case None =>
        Map.empty[String, FileType] ++ results
  }

  private def loadNameTypeCache(): Unit = {
    _nameTypeCache = getFileTypesForQueryAndParams("SELECT name, file_type_id FROM file_name", List.empty)
    _nameTypeCacheLoaded = true
  }

  private def getFileTypeForQueryAndParams(query: String, params: List[String]): FileType = {
    getConnection match
      case Some(conn) =>
        try {
          val stmt = conn.prepareStatement(query)
          params.zipWithIndex.foreach { case (param, idx) => stmt.setString(idx + 1, param) }
          val rs = stmt.executeQuery()
          if (rs.next()) {
            val fileTypeId = rs.getInt("file_type_id") - 1
            fileTypes(fileTypeId)
          } else {
            FileType.Unknown
          }
        } catch {
          case e: SQLException =>
            Common.logError(e.getMessage)
            FileType.Unknown
        }
      case None =>
        FileType.Unknown
  }

  private def getFileTypeForFileName(fileName: String): FileType = {
    if (!_nameTypeCacheLoaded) loadNameTypeCache()
    if (_nameTypeCache.contains(fileName)) return _nameTypeCache(fileName)
//    getFileTypeForQueryAndParams(SELECT_FILE_TYPE_ID_FOR_FILE_NAME, List(fileName))
    FileType.Unknown
  }

  private def getFileTypeForExtension(fileExt: String): FileType = {
    if (fileExt.isBlank) return FileType.Unknown
    if (_extTypeCache.contains(fileExt)) return _extTypeCache(fileExt)
    val fileType = getFileTypeForQueryAndParams(SELECT_FILE_TYPE_ID_FOR_EXTENSION, List(fileExt))
    _extTypeCache += (fileExt -> fileType)
    fileType
  }

  def getFileType(path: Path): FileType = {
    val fileTypeForFileName = getFileTypeForFileName(path.getFileName.toString)
    if (fileTypeForFileName != FileType.Unknown) {
      return fileTypeForFileName
    }
    val extension = FileUtil.getExtension(path)
    getFileTypeForExtension(extension)
  }

  def isArchiveFile(path: Path): Boolean = {
    getFileType(path) == FileType.Archive
  }

  def isAudioFile(path: Path): Boolean = {
    getFileType(path) == FileType.Audio
  }

  def isBinaryFile(path: Path): Boolean = {
    getFileType(path) == FileType.Binary
  }

  def isCodeFile(path: Path): Boolean = {
    getFileType(path) == FileType.Code
  }

  def isFontFile(path: Path): Boolean = {
    getFileType(path) == FileType.Font
  }

  def isImageFile(path: Path): Boolean = {
    getFileType(path) == FileType.Image
  }

  def isTextFile(path: Path): Boolean = {
    val fileType = getFileType(path)
    fileType == FileType.Text || fileType == FileType.Code || fileType == FileType.Xml
  }

  def isUnknownFile(path: Path): Boolean = {
    getFileType(path) == FileType.Unknown
  }

  def isVideoFile(path: Path): Boolean = {
    getFileType(path) == FileType.Video
  }

  def isXmlFile(path: Path): Boolean = {
    getFileType(path) == FileType.Xml
  }
}

object FileTypes {
  def isZipArchiveFile(path: Path): Boolean = {
    Set("zip", "jar", "war").contains(FileUtil.getExtension(path))
  }

  def isGzArchiveFile(path: Path): Boolean = {
    Set("gz", "tgz").contains(FileUtil.getExtension(path))
  }

  def isBz2ArchiveFile(path: Path): Boolean = {
    "bz2" == FileUtil.getExtension(path)
  }

  def isTarArchiveFile(path: Path): Boolean = {
    "tar" == FileUtil.getExtension(path)
  }
}

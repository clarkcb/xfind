package scalafind

import org.sqlite.SQLiteConfig
import scalafind.FileType.FileType

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
  private var _fileTypeCache = Map[String, FileType]()

  private def getConnection: Option[Connection] = {
    Class.forName("org.sqlite.JDBC")
    _conn match {
      case Some(c) => _conn
      case None =>
        try {
          val config = SQLiteConfig()
          config.setReadOnly(true)
          _conn = Some(DriverManager.getConnection("jdbc:sqlite:" + FindConfig.XFINDDB, config.toProperties))
          _conn
        } catch {
          case e: Exception =>
            Common.logError(e.getMessage)
            None
        }
    }
  }

  private def getFileTypeForQueryAndElem(query: String, elem: String): FileType = {
    getConnection match
      case Some(conn) =>
        try {
          val stmt = conn.prepareStatement(query)
          stmt.setString(1, elem)
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
    getFileTypeForQueryAndElem(SELECT_FILE_TYPE_ID_FOR_FILE_NAME, fileName)
  }

  private def getFileTypeForExtension(fileExt: String): FileType = {
    if (_fileTypeCache.contains(fileExt)) return _fileTypeCache(fileExt)
    val fileType = getFileTypeForQueryAndElem(SELECT_FILE_TYPE_ID_FOR_EXTENSION, fileExt)
    _fileTypeCache += (fileExt -> fileType)
    fileType
  }

  def getFileType(fileName: String): FileType = {
    val fileTypeForFileName = getFileTypeForFileName(fileName)
    if (fileTypeForFileName != FileType.Unknown) {
      return fileTypeForFileName
    }
    val extension = FileUtil.getExtension(fileName)
    getFileTypeForExtension(extension)
  }

  def isArchiveFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Archive
  }

  def isAudioFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Audio
  }

  def isBinaryFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Binary
  }

  def isCodeFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Code
  }

  def isFontFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Font
  }

  def isImageFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Image
  }

  def isTextFile(fileName: String): Boolean = {
    val fileType = getFileType(fileName)
    fileType == FileType.Text || fileType == FileType.Code || fileType == FileType.Xml
  }

  def isUnknownFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Unknown
  }

  def isVideoFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Video
  }

  def isXmlFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Xml
  }
}

object FileTypes {
  def isZipArchiveFile(fr: FileResult): Boolean = {
    Set("zip", "jar", "war").contains(FileUtil.getExtension(fr))
  }

  def isGzArchiveFile(fr: FileResult): Boolean = {
    Set("gz", "tgz").contains(FileUtil.getExtension(fr))
  }

  def isBz2ArchiveFile(fr: FileResult): Boolean = {
    "bz2" == FileUtil.getExtension(fr)
  }

  def isTarArchiveFile(fr: FileResult): Boolean = {
    "tar" == FileUtil.getExtension(fr)
  }
}

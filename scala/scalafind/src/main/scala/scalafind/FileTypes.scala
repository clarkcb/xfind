package scalafind

import org.json.{JSONObject, JSONTokener}

import java.io.IOException
import java.nio.file.Path
import scala.collection.mutable
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

object FileTypes {
  private val _fileTypesJsonPath = "/filetypes.json"

  private val fileTypeMaps: (Map[String, Set[String]], Map[String, Set[String]]) = {
    val _fileTypeExtMap = mutable.Map.empty[String, Set[String]]
    val _fileTypeNameMap = mutable.Map.empty[String, Set[String]]
    val fileTypesInputStream = getClass.getResourceAsStream(_fileTypesJsonPath)
    try {
      val jsonObj = new JSONObject(new JSONTokener(fileTypesInputStream))
      val fileTypesArray = jsonObj.getJSONArray("filetypes").iterator()
      while (fileTypesArray.hasNext) {
        val fileTypeObj = fileTypesArray.next().asInstanceOf[JSONObject]
        val typeName = fileTypeObj.getString("type")
        _fileTypeExtMap(typeName) = fileTypeObj.getJSONArray("extensions").toList.asScala.map(_.asInstanceOf[String]).toSet
        _fileTypeNameMap(typeName) = fileTypeObj.getJSONArray("names").toList.asScala.map(_.asInstanceOf[String]).toSet
      }
    } catch {
      case e: IOException =>
        print(e.getMessage)
    }

    _fileTypeExtMap(FileType.Text.toString) = _fileTypeExtMap(FileType.Text.toString) ++ _fileTypeExtMap(FileType.Code.toString) ++
      _fileTypeExtMap(FileType.Xml.toString)
    _fileTypeNameMap(FileType.Text.toString) = _fileTypeNameMap(FileType.Text.toString) ++ _fileTypeNameMap(FileType.Code.toString) ++
      _fileTypeNameMap(FileType.Xml.toString)
    (Map.empty[String, Set[String]] ++ _fileTypeExtMap, Map.empty[String, Set[String]] ++ _fileTypeNameMap)
  }
  private val fileTypeExtMap: Map[String, Set[String]] = fileTypeMaps._1
  private val fileTypeNameMap: Map[String, Set[String]] = fileTypeMaps._2

  def getFileType(path: Path): FileType.Value = {
    // most specific types first
    if (isCodeFile(path)) {
      FileType.Code
    } else if (isArchiveFile(path)) {
      FileType.Archive
    } else if (isAudioFile(path)) {
      FileType.Audio
    } else if (isFontFile(path)) {
      FileType.Font
    } else if (isImageFile(path)) {
      FileType.Image
    } else if (isVideoFile(path)) {
      FileType.Video

      // most general types last
    } else if (isXmlFile(path)) {
      FileType.Xml
    } else if (isTextFile(path)) {
      FileType.Text
    } else if (isBinaryFile(path)) {
      FileType.Binary
    } else {
      FileType.Unknown
    }
  }

  def isArchiveFile(path: Path): Boolean = {
    fileTypeNameMap(FileType.Archive.toString).contains(path.getFileName.toString)
      || fileTypeExtMap(FileType.Archive.toString).contains(FileUtil.getExtension(path))
  }

  def isAudioFile(path: Path): Boolean = {
    fileTypeNameMap(FileType.Audio.toString).contains(path.getFileName.toString)
      || fileTypeExtMap(FileType.Audio.toString).contains(FileUtil.getExtension(path))
  }

  def isBinaryFile(path: Path): Boolean = {
    fileTypeNameMap(FileType.Binary.toString).contains(path.getFileName.toString)
      || fileTypeExtMap(FileType.Binary.toString).contains(FileUtil.getExtension(path))
  }

  def isCodeFile(path: Path): Boolean = {
    fileTypeNameMap(FileType.Code.toString).contains(path.getFileName.toString)
      || fileTypeExtMap(FileType.Code.toString).contains(FileUtil.getExtension(path))
  }

  def isFontFile(path: Path): Boolean = {
    fileTypeNameMap(FileType.Font.toString).contains(path.getFileName.toString)
      || fileTypeExtMap(FileType.Font.toString).contains(FileUtil.getExtension(path))
  }

  def isImageFile(path: Path): Boolean = {
    fileTypeNameMap(FileType.Image.toString).contains(path.getFileName.toString)
      || fileTypeExtMap(FileType.Image.toString).contains(FileUtil.getExtension(path))
  }

  def isTextFile(path: Path): Boolean = {
    fileTypeNameMap(FileType.Text.toString).contains(path.getFileName.toString)
      || fileTypeExtMap(FileType.Text.toString).contains(FileUtil.getExtension(path))
  }

  def isUnknownFile(path: Path): Boolean = {
    getFileType(path) == FileType.Unknown
  }

  def isVideoFile(path: Path): Boolean = {
    fileTypeNameMap(FileType.Video.toString).contains(path.getFileName.toString)
      || fileTypeExtMap(FileType.Video.toString).contains(FileUtil.getExtension(path))
  }

  def isXmlFile(path: Path): Boolean = {
    fileTypeNameMap(FileType.Xml.toString).contains(path.getFileName.toString)
      || fileTypeExtMap(FileType.Xml.toString).contains(FileUtil.getExtension(path))
  }

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

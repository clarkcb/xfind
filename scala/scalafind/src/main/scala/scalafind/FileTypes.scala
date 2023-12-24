package scalafind

import org.json.{JSONObject, JSONTokener}

import java.io.{IOException, InputStreamReader}
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

  def getFileType(fileName: String): FileType.Value = {
    // most specific types first
    if (isCodeFile(fileName)) {
      FileType.Code
    } else if (isArchiveFile(fileName)) {
      FileType.Archive
    } else if (isAudioFile(fileName)) {
      FileType.Audio
    } else if (isFontFile(fileName)) {
      FileType.Font
    } else if (isImageFile(fileName)) {
      FileType.Image
    } else if (isVideoFile(fileName)) {
      FileType.Video

      // most general types last
    } else if (isXmlFile(fileName)) {
      FileType.Xml
    } else if (isTextFile(fileName)) {
      FileType.Text
    } else if (isBinaryFile(fileName)) {
      FileType.Binary
    } else {
      FileType.Unknown
    }
  }

  def isArchiveFile(fileName: String): Boolean = {
    fileTypeNameMap(FileType.Archive.toString).contains(fileName)
      || fileTypeExtMap(FileType.Archive.toString).contains(FileUtil.getExtension(fileName))
  }

  def isAudioFile(fileName: String): Boolean = {
    fileTypeNameMap(FileType.Audio.toString).contains(fileName)
      || fileTypeExtMap(FileType.Audio.toString).contains(FileUtil.getExtension(fileName))
  }

  def isBinaryFile(fileName: String): Boolean = {
    fileTypeNameMap(FileType.Binary.toString).contains(fileName)
      || fileTypeExtMap(FileType.Binary.toString).contains(FileUtil.getExtension(fileName))
  }

  def isCodeFile(fileName: String): Boolean = {
    fileTypeNameMap(FileType.Code.toString).contains(fileName)
      || fileTypeExtMap(FileType.Code.toString).contains(FileUtil.getExtension(fileName))
  }

  def isFontFile(fileName: String): Boolean = {
    fileTypeNameMap(FileType.Font.toString).contains(fileName)
      || fileTypeExtMap(FileType.Font.toString).contains(FileUtil.getExtension(fileName))
  }

  def isImageFile(fileName: String): Boolean = {
    fileTypeNameMap(FileType.Image.toString).contains(fileName)
      || fileTypeExtMap(FileType.Image.toString).contains(FileUtil.getExtension(fileName))
  }

  def isTextFile(fileName: String): Boolean = {
    fileTypeNameMap(FileType.Text.toString).contains(fileName)
      || fileTypeExtMap(FileType.Text.toString).contains(FileUtil.getExtension(fileName))
  }

  def isUnknownFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Unknown
  }

  def isVideoFile(fileName: String): Boolean = {
    fileTypeNameMap(FileType.Video.toString).contains(fileName)
      || fileTypeExtMap(FileType.Video.toString).contains(FileUtil.getExtension(fileName))
  }

  def isXmlFile(fileName: String): Boolean = {
    fileTypeNameMap(FileType.Xml.toString).contains(fileName)
      || fileTypeExtMap(FileType.Xml.toString).contains(FileUtil.getExtension(fileName))
  }

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

package scalafind

import org.json.{JSONObject, JSONTokener}

import java.io.{IOException, InputStreamReader}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object FileType extends Enumeration {
  type FileType = Value
  val Unknown: FileType = Value
  val Archive: FileType = Value
  val Audio: FileType   = Value
  val Binary: FileType  = Value
  val Code: FileType    = Value
  val Font: FileType    = Value
  val Image: FileType   = Value
  val Text: FileType    = Value
  val Video: FileType   = Value
  val Xml: FileType     = Value
}

object FileTypes {
  private val _fileTypesJsonPath = "/filetypes.json"

  private val archive = "archive"
  private val audio = "audio"
  private val binary = "binary"
  private val code = "code"
  private val font = "font"
  private val image = "image"
  private val searchable = "searchable"
  private val text = "text"
  private val unknown = "unknown"
  private val video = "video"
  private val xml = "xml"

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

    _fileTypeExtMap(text) = _fileTypeExtMap(text) ++ _fileTypeExtMap(code) ++
      _fileTypeExtMap(xml)
    _fileTypeNameMap(text) = _fileTypeNameMap(text) ++ _fileTypeNameMap(code) ++
      _fileTypeNameMap(xml)
    (Map.empty[String, Set[String]] ++ _fileTypeExtMap, Map.empty[String, Set[String]] ++ _fileTypeNameMap)
  }
  private val fileTypeExtMap: Map[String, Set[String]] = fileTypeMaps._1
  private val fileTypeNameMap: Map[String, Set[String]] = fileTypeMaps._2

  def fromName(name: String): FileType.Value = {
    val lname = name.toLowerCase
    if (lname == archive) {
      FileType.Archive
    } else if (lname == audio) {
      FileType.Audio
    } else if (lname == binary) {
      FileType.Binary
    } else if (lname == code) {
      FileType.Code
    } else if (lname == font) {
      FileType.Font
    } else if (lname == image) {
      FileType.Image
    } else if (lname == text) {
      FileType.Text
    } else if (lname == video) {
      FileType.Video
    } else if (lname == xml) {
      FileType.Xml
    } else {
      FileType.Unknown
    }
  }

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
    fileTypeNameMap(archive).contains(fileName)
      || fileTypeExtMap(archive).contains(FileUtil.getExtension(fileName))
  }

  def isAudioFile(fileName: String): Boolean = {
    fileTypeNameMap(audio).contains(fileName)
      || fileTypeExtMap(audio).contains(FileUtil.getExtension(fileName))
  }

  def isBinaryFile(fileName: String): Boolean = {
    fileTypeNameMap(binary).contains(fileName)
      || fileTypeExtMap(binary).contains(FileUtil.getExtension(fileName))
  }

  def isCodeFile(fileName: String): Boolean = {
    fileTypeNameMap(code).contains(fileName)
      || fileTypeExtMap(code).contains(FileUtil.getExtension(fileName))
  }

  def isFontFile(fileName: String): Boolean = {
    fileTypeNameMap(font).contains(fileName)
      || fileTypeExtMap(font).contains(FileUtil.getExtension(fileName))
  }

  def isImageFile(fileName: String): Boolean = {
    fileTypeNameMap(image).contains(fileName)
      || fileTypeExtMap(image).contains(FileUtil.getExtension(fileName))
  }

  def isTextFile(fileName: String): Boolean = {
    fileTypeNameMap(text).contains(fileName)
      || fileTypeExtMap(text).contains(FileUtil.getExtension(fileName))
  }

  def isUnknownFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Unknown
  }

  def isVideoFile(fileName: String): Boolean = {
    fileTypeNameMap(video).contains(fileName)
      || fileTypeExtMap(video).contains(FileUtil.getExtension(fileName))
  }

  def isXmlFile(fileName: String): Boolean = {
    fileTypeNameMap(xml).contains(fileName)
      || fileTypeExtMap(xml).contains(FileUtil.getExtension(fileName))
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

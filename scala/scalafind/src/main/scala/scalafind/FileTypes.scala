package scalafind

import org.json.simple.parser.{JSONParser, ParseException}
import org.json.simple.{JSONArray, JSONObject}

import java.io.{IOException, InputStreamReader}
import scala.collection.mutable

object FileType extends Enumeration {
  type FileType = Value
  val Unknown: FileType = Value
  val Archive: FileType = Value
  val Binary: FileType  = Value
  val Code: FileType    = Value
  val Text: FileType    = Value
  val Xml: FileType     = Value
}

object FileTypes {
  private val _fileTypesJsonPath = "/filetypes.json"

  private val archive = "archive"
  private val binary = "binary"
  private val code = "code"
  private val searchable = "searchable"
  private val text = "text"
  private val unknown = "unknown"
  private val xml = "xml"

  private val fileTypeMaps: (Map[String, Set[String]], Map[String, Set[String]]) = {
    val _fileTypeExtMap = mutable.Map.empty[String, Set[String]]
    val _fileTypeNameMap = mutable.Map.empty[String, Set[String]]
    val fileTypesInputStream = getClass.getResourceAsStream(_fileTypesJsonPath)
    try {
      val obj = new JSONParser().parse(new InputStreamReader(fileTypesInputStream))
      val jsonObj = obj.asInstanceOf[JSONObject]
      val ftIt = jsonObj.get("filetypes").asInstanceOf[JSONArray].iterator()
      while (ftIt.hasNext) {
        val ftObj = ftIt.next().asInstanceOf[JSONObject]
        val typeName = ftObj.get("type").asInstanceOf[String]
        val exSet = mutable.Set.empty[String]
        val exIt = ftObj.get("extensions").asInstanceOf[JSONArray].iterator()
        while (exIt.hasNext) {
           exSet += exIt.next().asInstanceOf[String]
        }
         _fileTypeExtMap(typeName) = Set.empty[String] ++ exSet
        val nameSet = mutable.Set.empty[String]
        val nameIt = ftObj.get("names").asInstanceOf[JSONArray].iterator()
        while (nameIt.hasNext) {
           nameSet += nameIt.next().asInstanceOf[String]
        }
         _fileTypeNameMap(typeName) = Set.empty[String] ++ nameSet
      }
    } catch {
      case e: ParseException =>
        print(e.getMessage)
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
    if (lname == code) {
      FileType.Code
    } else if (lname == binary) {
      FileType.Binary
    } else if (lname == archive) {
      FileType.Archive
    } else if (lname == xml) {
      FileType.Xml
    } else if (lname == text) {
      FileType.Text
    } else {
      FileType.Unknown
    }
  }

  def getFileType(fileName: String): FileType.Value = {
    if (isCodeFile(fileName)) {
      FileType.Code
    } else if (isXmlFile(fileName)) {
      FileType.Xml
    } else if (isTextFile(fileName)) {
      FileType.Text
    } else if (isBinaryFile(fileName)) {
      FileType.Binary
    } else if (isArchiveFile(fileName)) {
      FileType.Archive
    } else {
      FileType.Unknown
    }
  }

  def isArchiveFile(fileName: String): Boolean = {
    fileTypeNameMap(archive).contains(fileName)
      || fileTypeExtMap(archive).contains(FileUtil.getExtension(fileName))
  }

  def isBinaryFile(fileName: String): Boolean = {
    fileTypeNameMap(binary).contains(fileName)
      || fileTypeExtMap(binary).contains(FileUtil.getExtension(fileName))
  }

  def isCodeFile(fileName: String): Boolean = {
    fileTypeNameMap(code).contains(fileName)
      || fileTypeExtMap(code).contains(FileUtil.getExtension(fileName))
  }

  def isTextFile(fileName: String): Boolean = {
    fileTypeNameMap(text).contains(fileName)
      || fileTypeExtMap(text).contains(FileUtil.getExtension(fileName))
  }

  def isUnknownFile(fileName: String): Boolean = {
    getFileType(fileName) == FileType.Unknown
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

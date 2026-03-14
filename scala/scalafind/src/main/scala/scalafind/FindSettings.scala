package scalafind

import scalafind.Color.Color
import scalafind.FileType.FileType
import scalafind.SortBy.SortBy

import java.lang.reflect.{Field, ParameterizedType, Type}
import java.nio.file.Path
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{LocalDate, LocalDateTime}
import scala.annotation.tailrec
import scala.util.matching.Regex

object SortBy extends Enumeration {
  type SortBy = Value
  val FileName: SortBy = Value("filename")
  val FilePath: SortBy = Value("filepath")
  val FileSize: SortBy = Value("filesize")
  val FileType: SortBy = Value("filetype")
  val LastMod: SortBy = Value("lastmod")

  def forName(sortByName: String): SortBy = {
    sortByName.toLowerCase() match {
      case "filename" => SortBy.FileName
      case "name" => SortBy.FileName
      case "filesize" => SortBy.FileSize
      case "size" => SortBy.FileSize
      case "filetype" => SortBy.FileType
      case "type" => SortBy.FileType
      case "lastmod" => SortBy.LastMod
      case _ => SortBy.FilePath
    }
  }
}

object DefaultFindSettings {
  val archivesOnly = false
  val colorize = true
  val debug = false
  val defaultFiles = true
  val dirColor: Color = Color.CYAN
  val extColor: Color = Color.YELLOW
  val fileColor: Color = Color.MAGENTA
  val followSymlinks = false
  var includeArchives = false
  val includeHidden = false
  val maxDepth: Int = -1
  val maxSize: Long = 0L
  val minDepth: Int = -1
  val minSize: Long = 0L
  var paths: Set[Path] = Set.empty[Path]
  val printDirs = false
  val printFiles = false
  var printUsage = false
  var printVersion = false
  var recursive = true
  var sortBy: SortBy = SortBy.FilePath
  var sortCaseInsensitive = false
  var sortDescending = false
  var verbose = false
}

case class FindSettings(archivesOnly: Boolean = DefaultFindSettings.archivesOnly,
                        colorize: Boolean = DefaultFindSettings.colorize,
                        debug: Boolean = DefaultFindSettings.debug,
                        defaultFiles: Boolean = DefaultFindSettings.defaultFiles,
                        dirColor: Color = DefaultFindSettings.dirColor,
                        extColor: Color = DefaultFindSettings.extColor,
                        fileColor: Color = DefaultFindSettings.fileColor,
                        followSymlinks: Boolean = DefaultFindSettings.followSymlinks,
                        inArchiveExtensions: Set[String] = Set.empty[String],
                        inArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                        inDirPatterns: Set[Regex] = Set.empty[Regex],
                        inExtensions: Set[String] = Set.empty[String],
                        inFilePatterns: Set[Regex] = Set.empty[Regex],
                        inFileTypes: Set[FileType] = Set.empty[FileType],
                        var includeArchives: Boolean = DefaultFindSettings.includeArchives,
                        includeHidden: Boolean = DefaultFindSettings.includeHidden,
                        maxDepth: Int = DefaultFindSettings.maxDepth,
                        maxLastMod: Option[LocalDateTime] = None,
                        maxSize: Long = DefaultFindSettings.maxSize,
                        minDepth: Int = DefaultFindSettings.minDepth,
                        minLastMod: Option[LocalDateTime] = None,
                        minSize: Long = DefaultFindSettings.minSize,
                        outArchiveExtensions: Set[String] = Set.empty[String],
                        outArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                        outDirPatterns: Set[Regex] = Set.empty[Regex],
                        outExtensions: Set[String] = Set.empty[String],
                        outFilePatterns: Set[Regex] = Set.empty[Regex],
                        outFileTypes: Set[FileType] = Set.empty[FileType],
                        paths: Set[Path] = DefaultFindSettings.paths,
                        printDirs: Boolean = DefaultFindSettings.printDirs,
                        printFiles: Boolean = DefaultFindSettings.printFiles,
                        printUsage: Boolean = DefaultFindSettings.printUsage,
                        printVersion: Boolean = DefaultFindSettings.printVersion,
                        recursive: Boolean = DefaultFindSettings.recursive,
                        sortBy: SortBy = DefaultFindSettings.sortBy,
                        sortCaseInsensitive: Boolean = DefaultFindSettings.sortCaseInsensitive,
                        sortDescending: Boolean = DefaultFindSettings.sortDescending,
                        var verbose: Boolean = DefaultFindSettings.verbose) {

  includeArchives = archivesOnly || includeArchives
  verbose = debug || verbose

  def addExtensions(exts: String, extensions: Set[String]): Set[String] = {
    extensions ++ exts.split(",").filterNot(_.isEmpty)
  }

  def getLastModFromString(lastModString: String): Option[LocalDateTime] = {
    try {
      Some(LocalDateTime.parse(lastModString))
    } catch {
      case _: DateTimeParseException => try {
        val maxLastModDate = LocalDate.parse(lastModString, DateTimeFormatter.ISO_LOCAL_DATE)
        Some(maxLastModDate.atTime(0, 0, 0))
      } catch {
        case _: Exception => None
      }
      case _: Exception => None
    }
  }

  def needLastMod: Boolean = {
    sortBy == SortBy.LastMod
      || maxLastMod.nonEmpty || minLastMod.nonEmpty
  }

  def needSize: Boolean = {
    sortBy == SortBy.FileSize
      || maxSize > 0 || minSize > 0
  }

  private def setToString(set: Set[String]): String = {
    if (set.isEmpty) "[]" else set.mkString("[\"", "\", \"", "\"]")
  }

  private def pathSetToString(set: Set[Path]): String = {
    if (set.isEmpty) "[]" else set.map(p => p.toString).mkString("[\"", "\", \"", "\"]")
  }

  private def regexSetToString(set: Set[Regex]): String = {
    if (set.isEmpty) "[]" else set.map(p => p.pattern.pattern()).mkString("[\"", "\", \"", "\"]")
  }

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder("FindSettings(")
    val fields: List[Field] = getClass.getDeclaredFields.filter((f: Field) => !f.getName.contains("_")).toList
    val totalFields = fields.length

    @tailrec
    def recAddFields(fields: List[Field]): Unit = fields match {
      case Nil => ()
      case f :: fs =>
        f.setAccessible(true)
        if (fields.length < totalFields) sb.append(", ")
        sb.append(f.getName).append("=")
        val typeName: String = f.getType.getName
        f.getType.getName match {
          case "javafind.Color" =>
            sb.append(f.get(this).asInstanceOf[Color].toString)
          case "javafind.SortBy" =>
            sb.append(f.get(this).asInstanceOf[SortBy].toString)
          case "scala.collection.immutable.Set" =>
            val actualTypeArguments: Array[Type] = f.getGenericType.asInstanceOf[ParameterizedType].getActualTypeArguments
            if (actualTypeArguments.length > 0) {
              actualTypeArguments(0).getTypeName match {
                case "java.lang.String" =>
                  sb.append(setToString(f.get(this).asInstanceOf[Set[String]]))
                case "scala.util.matching.Regex" =>
                  sb.append(regexSetToString(f.get(this).asInstanceOf[Set[Regex]]))
                case "javafind.FileType" =>
                  val fts = f.get(this).asInstanceOf[Set[FileType]]
                  sb.append(fts.map(ft => ft.toString).mkString("[", ", ", "]"))
                case "java.nio.file.Path" =>
                  sb.append(pathSetToString(f.get(this).asInstanceOf[Set[Path]]))
                case _ =>
              }
            }
          case "scala.Option" =>
            val actualTypeArguments: Array[Type] = f.getGenericType.asInstanceOf[ParameterizedType].getActualTypeArguments
            if (actualTypeArguments.length > 0) {
              actualTypeArguments(0).getTypeName match {
                case "java.time.LocalDateTime" =>
                  val dt = f.get(this).asInstanceOf[Option[LocalDateTime]]
                  sb.append(if (dt.isEmpty) "0" else dt.get)
                case _ =>
              }
            }
          case _ =>
            sb.append(f.get(this))
        }
        recAddFields(fs)
    }

    recAddFields(fields)
    sb.append(")")
    sb.toString
  }
}

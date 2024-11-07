package scalafind

import scalafind.FileType.FileType
import scalafind.SortBy.SortBy

import java.nio.file.Path
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{LocalDate, LocalDateTime}
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
  val debug = false
  val followSymlinks = false
  var includeArchives = false
  val includeHidden = false
  val maxDepth: Int = -1
  val maxSize = 0
  val minDepth: Int = -1
  val minSize = 0
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
                        debug: Boolean = DefaultFindSettings.debug,
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
                        maxSize: Int = DefaultFindSettings.maxSize,
                        minDepth: Int = DefaultFindSettings.minDepth,
                        minLastMod: Option[LocalDateTime] = None,
                        minSize: Int = DefaultFindSettings.minSize,
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
    "FindSettings(" +
      "archivesOnly=" + archivesOnly +
      ", debug=" + debug +
      ", followSymlinks=" + followSymlinks +
      ", inArchiveExtensions=" + setToString(inArchiveExtensions) +
      ", inArchiveFilePatterns=" + regexSetToString(inArchiveFilePatterns) +
      ", inDirPatterns=" + regexSetToString(inDirPatterns) +
      ", inExtensions=" + setToString(inExtensions) +
      ", inFilePatterns=" + regexSetToString(inFilePatterns) +
      ", inFileTypes=" + inFileTypes.map(ft => ft.toString).mkString("[", ", ", "]") +
      ", includeArchives=" + includeArchives +
      ", includeHidden=" + includeHidden +
      ", maxDepth=" + maxDepth +
      ", maxLastMod=" + (if (maxLastMod.isEmpty) "0" else maxLastMod) +
      ", maxSize=" + maxSize +
      ", minDepth=" + minDepth +
      ", minLastMod=" + (if (minLastMod.isEmpty) "0" else minLastMod) +
      ", minSize=" + minSize +
      ", outArchiveExtensions=" + setToString(outArchiveExtensions) +
      ", outArchiveFilePatterns=" + regexSetToString(outArchiveFilePatterns) +
      ", outDirPatterns=" + regexSetToString(outDirPatterns) +
      ", outExtensions=" + setToString(outExtensions) +
      ", outFilePatterns=" + regexSetToString(outFilePatterns) +
      ", outFileTypes=" + outFileTypes.map(ft => ft.toString).mkString("[", ", ", "]") +
      ", paths=" + pathSetToString(paths) +
      ", printDirs=" + printDirs +
      ", printFiles=" + printFiles +
      ", printUsage=" + printUsage +
      ", printVersion=" + printVersion +
      ", recursive=" + recursive +
      ", sortBy=" + sortBy.toString +
      ", sortCaseInsensitive=" + sortCaseInsensitive +
      ", sortDescending=" + sortDescending +
      ", verbose=" + verbose +
      ")"
  }
}

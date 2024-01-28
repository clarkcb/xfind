package scalafind

import scalafind.FileType.FileType
import scalafind.SortBy.SortBy
import java.time.LocalDateTime

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
  var includeArchives = false
  val includeHidden = false
  val maxDepth: Int = -1
  val maxSize = 0
  val minDepth: Int = -1
  val minSize = 0
  var paths: Set[String] = Set.empty[String]
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
                        paths: Set[String] = DefaultFindSettings.paths,
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

  def needStat: Boolean = {
    sortBy == SortBy.FileSize || sortBy == SortBy.LastMod
      || maxLastMod.nonEmpty || minLastMod.nonEmpty
      || maxSize > 0 || minSize > 0
  }

  def setToString(set: Set[String]): String = {
    if (set.isEmpty) "[]" else set.mkString("[\"", "\", \"", "\"]")
  }

  def regexSetToString(set: Set[Regex]): String = {
    if (set.isEmpty) "[]" else set.map(p => p.pattern.pattern()).mkString("[\"", "\", \"", "\"]")
  }

  override def toString: String = {
    "FindSettings(" +
      "archivesOnly=" + archivesOnly +
      ", debug=" + debug +
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
      ", paths=" + setToString(paths) +
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

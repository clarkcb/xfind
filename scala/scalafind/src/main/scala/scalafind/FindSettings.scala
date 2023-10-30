package scalafind

import scalafind.FileType.FileType
import scalafind.SortBy.SortBy
import java.time.LocalDateTime

import scala.util.matching.Regex

object SortBy extends Enumeration {
  type SortBy = Value
  val FileName: SortBy = Value
  val FilePath: SortBy = Value
  val FileSize: SortBy = Value
  val FileType: SortBy = Value
  val LastMod: SortBy = Value

  def fromName(sortByName: String): SortBy = {
    sortByName.toLowerCase() match {
      case "name" => SortBy.FileName
      case "size" => SortBy.FileSize
      case "type" => SortBy.FileType
      case "lastmod" => SortBy.LastMod
      case _ => SortBy.FilePath
    }
  }
}

object DefaultSettings {
  val archivesOnly = false
  val debug = false
  var includeArchives = false
  val includeHidden = false
  val listDirs = false
  val listFiles = false
  val maxDepth: Int = -1
  val maxSize = 0
  val minDepth: Int = -1
  val minSize = 0
  var paths: Set[String] = Set.empty[String]
  var printUsage = false
  var printVersion = false
  var recursive = true
  var sortBy: SortBy = SortBy.FilePath
  var sortCaseInsensitive = false
  var sortDescending = false
  var verbose = false
}

case class FindSettings(archivesOnly: Boolean = DefaultSettings.archivesOnly,
                        debug: Boolean = DefaultSettings.debug,
                        inArchiveExtensions: Set[String] = Set.empty[String],
                        inArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                        inDirPatterns: Set[Regex] = Set.empty[Regex],
                        inExtensions: Set[String] = Set.empty[String],
                        inFilePatterns: Set[Regex] = Set.empty[Regex],
                        inFileTypes: Set[FileType] = Set.empty[FileType],
                        var includeArchives: Boolean = DefaultSettings.includeArchives,
                        includeHidden: Boolean = DefaultSettings.includeHidden,
                        listDirs: Boolean = DefaultSettings.listDirs,
                        listFiles: Boolean = DefaultSettings.listFiles,
                        maxDepth: Int = DefaultSettings.maxDepth,
                        maxLastMod: Option[LocalDateTime] = None,
                        maxSize: Int = DefaultSettings.maxSize,
                        minDepth: Int = DefaultSettings.minDepth,
                        minLastMod: Option[LocalDateTime] = None,
                        minSize: Int = DefaultSettings.minSize,
                        outArchiveExtensions: Set[String] = Set.empty[String],
                        outArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                        outDirPatterns: Set[Regex] = Set.empty[Regex],
                        outExtensions: Set[String] = Set.empty[String],
                        outFilePatterns: Set[Regex] = Set.empty[Regex],
                        outFileTypes: Set[FileType] = Set.empty[FileType],
                        paths: Set[String] = DefaultSettings.paths,
                        printUsage: Boolean = DefaultSettings.printUsage,
                        printVersion: Boolean = DefaultSettings.printVersion,
                        recursive: Boolean = DefaultSettings.recursive,
                        sortBy: SortBy = DefaultSettings.sortBy,
                        sortCaseInsensitive: Boolean = DefaultSettings.sortCaseInsensitive,
                        sortDescending: Boolean = DefaultSettings.sortDescending,
                        var verbose: Boolean = DefaultSettings.verbose) {

  includeArchives = archivesOnly || includeArchives
  verbose = debug || verbose

  def needStat: Boolean = {
    sortBy == SortBy.FileSize || sortBy == SortBy.LastMod
      || maxLastMod.nonEmpty || minLastMod.nonEmpty
      || maxSize > 0 || minSize > 0
  }

  override def toString: String = {
    "FindSettings(" +
      "archivesOnly: " + archivesOnly +
      ", debug: " + debug +
      ", inArchiveExtensions: " + inArchiveExtensions +
      ", inArchiveFilePatterns: " + inArchiveFilePatterns +
      ", inDirPatterns: " + inDirPatterns +
      ", inExtensions: " + inExtensions +
      ", inFilePatterns: " + inFilePatterns +
      ", inFileTypes: " + inFileTypes +
      ", includeArchives: " + includeArchives +
      ", includeHidden: " + includeHidden +
      ", listDirs: " + listDirs +
      ", listFiles: " + listFiles +
      ", maxDepth: " + maxDepth +
      ", maxLastMod: " + maxLastMod +
      ", maxSize: " + maxSize +
      ", minDepth: " + minDepth +
      ", minLastMod: " + minLastMod +
      ", minSize: " + minSize +
      ", outArchiveExtensions: " + outArchiveExtensions +
      ", outArchiveFilePatterns: " + outArchiveFilePatterns +
      ", outDirPatterns: " + outDirPatterns +
      ", outExtensions: " + outExtensions +
      ", outFilePatterns: " + outFilePatterns +
      ", outFileTypes: " + outFileTypes +
      ", paths: " + paths  +
      ", printUsage: " + printUsage +
      ", printVersion: " + printVersion +
      ", recursive: " + recursive +
      ", sortBy: " + sortBy +
      ", sortCaseInsensitive: " + sortCaseInsensitive +
      ", sortDescending: " + sortDescending +
      ", verbose: " + verbose +
      ")"
  }
}

package scalafind

import scalafind.FileType.FileType

import scala.util.matching.Regex

object DefaultSettings {
  val archivesOnly = false
  val debug = false
  val excludeHidden = true
  var includeArchives = false
  val listDirs = false
  val listFiles = false
  var printUsage = false
  var printVersion = false
  var recursive = true
  var paths: Set[String] = Set.empty[String]
  var verbose = false
}

case class FindSettings(archivesOnly: Boolean = DefaultSettings.archivesOnly,
                          debug: Boolean = DefaultSettings.debug,
                          excludeHidden: Boolean = DefaultSettings.excludeHidden,
                          inArchiveExtensions: Set[String] = Set.empty[String],
                          inArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                          inDirPatterns: Set[Regex] = Set.empty[Regex],
                          inExtensions: Set[String] = Set.empty[String],
                          inFilePatterns: Set[Regex] = Set.empty[Regex],
                          inFileTypes: Set[FileType] = Set.empty[FileType],
                          var includeArchives: Boolean = DefaultSettings.includeArchives,
                          listDirs: Boolean = DefaultSettings.listDirs,
                          listFiles: Boolean = DefaultSettings.listFiles,
                          outArchiveExtensions: Set[String] = Set.empty[String],
                          outArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                          outDirPatterns: Set[Regex] = Set.empty[Regex],
                          outExtensions: Set[String] = Set.empty[String],
                          outFilePatterns: Set[Regex] = Set.empty[Regex],
                          outFileTypes: Set[FileType] = Set.empty[FileType],
                          printUsage: Boolean = DefaultSettings.printUsage,
                          printVersion: Boolean = DefaultSettings.printVersion,
                          recursive: Boolean = DefaultSettings.recursive,
                          paths: Set[String] = DefaultSettings.paths,
                          var verbose: Boolean = DefaultSettings.verbose) {

  includeArchives = archivesOnly || includeArchives
  verbose = debug || verbose

  override def toString: String = {
    "FindSettings(" +
      "archivesOnly: " + archivesOnly +
      ", debug: " + debug +
      ", excludeHidden: " + excludeHidden +
      ", inArchiveExtensions: " + inArchiveExtensions +
      ", inArchiveFilePatterns: " + inArchiveFilePatterns +
      ", inDirPatterns: " + inDirPatterns +
      ", inExtensions: " + inExtensions +
      ", inFilePatterns: " + inFilePatterns +
      ", inFileTypes: " + inFileTypes +
      ", includeArchives: " + includeArchives +
      ", listDirs: " + listDirs +
      ", listFiles: " + listFiles +
      ", outArchiveExtensions: " + outArchiveExtensions +
      ", outArchiveFilePatterns: " + outArchiveFilePatterns +
      ", outDirPatterns: " + outDirPatterns +
      ", outExtensions: " + outExtensions +
      ", outFilePatterns: " + outFilePatterns +
      ", outFileTypes: " + outFileTypes +
      ", printUsage: " + printUsage +
      ", printVersion: " + printVersion +
      ", recursive: " + recursive +
      ", paths: " + paths  +
      ", verbose: " + verbose +
      ")"
  }
}

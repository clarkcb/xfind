package scalafind

import scalafind.FileType.FileType
import scalafind.FileUtil.{getExtension, isHidden}

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Finder (settings: FindSettings) {
  import Finder.*

  private def validateSettings(): Unit = {
    settingsTests.foreach { t =>
      t(settings) match {
        case Some(err) => throw new FindException(err)
        case _ =>
      }
    }
  }
  validateSettings()

  def isMatchingDir(path: Path): Boolean = {
    val pathElems = FileUtil.splitPath(path)
    if (!settings.includeHidden && pathElems.exists(p => isHidden(p))) {
      false
    } else {
      filterByPatterns(path.toString, settings.inDirPatterns, settings.outDirPatterns)
    }
  }

  private def isMatchingExtension(ext: String, inExtensions: Set[String], outExtensions: Set[String]): Boolean = {
    (inExtensions.isEmpty || inExtensions.contains(ext))
      && (outExtensions.isEmpty || !outExtensions.contains(ext))
  }

  private def hasMatchingExtension(fr: FileResult, inExtensions: Set[String], outExtensions: Set[String]): Boolean = {
    if (inExtensions.nonEmpty || outExtensions.nonEmpty) {
      val ext = getExtension(fr.path.getFileName.toString)
      isMatchingExtension(ext, inExtensions, outExtensions)
    } else {
      true
    }
  }

  private def isMatchingFileName(fileName: String, inFilePatterns: Set[Regex], outFilePatterns: Set[Regex]): Boolean = {
    if (inFilePatterns.nonEmpty || outFilePatterns.nonEmpty) {
      (inFilePatterns.isEmpty || matchesAnyPattern(fileName, inFilePatterns))
        && (outFilePatterns.isEmpty || !matchesAnyPattern(fileName, outFilePatterns))
    } else {
      true
    }
  }

  private def isMatchingFileType(fileType: FileType): Boolean = {
    (settings.inFileTypes.isEmpty || settings.inFileTypes.contains(fileType))
      && (settings.outFileTypes.isEmpty || !settings.outFileTypes.contains(fileType))
  }

  private def isMatchingSize(fileSize: Long): Boolean = {
    (settings.maxSize <= 0 || fileSize <= settings.maxSize)
      && (settings.minSize <= 0 || fileSize >= settings.minSize)
  }

  private def isMatchingLastMod(lastMod: Option[Instant]): Boolean = {
    (settings.maxLastMod.isEmpty
      || lastMod.get.compareTo(settings.maxLastMod.get.toInstant(ZoneOffset.UTC)) <= 0)
      && (settings.minLastMod.isEmpty
      || lastMod.get.compareTo(settings.minLastMod.get.toInstant(ZoneOffset.UTC)) >= 0)
  }

  def isMatchingFileResult(fr: FileResult): Boolean = {
    hasMatchingExtension(fr, settings.inExtensions, settings.outExtensions)
      && isMatchingFileName(fr.path.getFileName.toString, settings.inFilePatterns, settings.outFilePatterns)
      && isMatchingFileType(fr.fileType)
      && isMatchingSize(fr.fileSize)
      && isMatchingLastMod(fr.lastMod.map(_.toInstant))
  }

  def isMatchingArchiveFileResult(fr: FileResult): Boolean = {
    hasMatchingExtension(fr, settings.inArchiveExtensions, settings.outArchiveExtensions)
      && isMatchingFileName(fr.path.getFileName.toString, settings.inArchiveFilePatterns, settings.outArchiveFilePatterns)
  }

  def filterToFileResult(p: Path): Option[FileResult] = {
    if (!settings.includeHidden && Files.isHidden(p)) {
      None
    } else {
      val (fileSize, lastMod) =
        if (settings.needLastMod || settings.needSize) {
          try {
            val stat = Files.readAttributes(p, classOf[BasicFileAttributes])
            val size = if (settings.needSize) stat.size() else 0
            val lm = if (settings.needLastMod) Some(stat.lastModifiedTime()) else None
            (size, lm)
          } catch {
            case _: IOException => (0L, None)
          }
        } else {
          (0L, None)
        }
      val fileResult = new FileResult(p, FileTypes.getFileType(p), fileSize, lastMod)
      fileResult.fileType match {
        // This is commented out to allow unknown files to match in case settings are permissive
        // case FileType.Unknown => None
        case FileType.Archive =>
          if (settings.includeArchives && isMatchingArchiveFileResult(fileResult)) {
            Some(fileResult)
          } else {
            None
          }
        case _ =>
          if (!settings.archivesOnly && isMatchingFileResult(fileResult)) {
            Some(fileResult)
          } else {
            None
          }
      }
    }
  }

  final def getDirFileResults(startPath: Path): Seq[FileResult] = {
    Files.list(startPath).iterator().asScala
      .filter(Files.isRegularFile(_))
      .map(filterToFileResult)
      .filter(_.nonEmpty)
      .flatten.toSeq
  }

  private final def filterDir(dirPath: Path, startPathSepCount: Int): Boolean = {
    if (dirPath == null) {
      true
    } else {
      val dirPathSepCount = FileUtil.sepCount(dirPath.toString)
      val depth = dirPathSepCount - startPathSepCount
      (settings.maxDepth < 1 || depth <= settings.maxDepth)
        && isMatchingDir(dirPath)
    }
  }

  private final def filterFile(filePath: Path, startPathSepCount: Int): Option[FileResult] = {
    val filePathSepCount = FileUtil.sepCount(filePath.toString)
    val depth = filePathSepCount - startPathSepCount
    if (depth < settings.minDepth || (settings.maxDepth > 0 && depth > settings.maxDepth)) {
      None
    } else {
      filterToFileResult(filePath)
    }
  }

  final def getFileResults(startPath: Path): Seq[FileResult] = {
    val startPathSepCount = FileUtil.sepCount(startPath.toString)
    val fileResults = Files.walk(startPath).iterator().asScala
      .filter(Files.isRegularFile(_))
      .filter(f => filterDir(f.getParent, startPathSepCount))
      .flatMap(f => filterFile(f, startPathSepCount))
      .toSeq
    fileResults
  }

  def cmpFileResults(fr1: FileResult, fr2: FileResult): Boolean = {
    if (settings.sortBy == SortBy.FileName) {
      fr1.compareByName(fr2, settings.sortCaseInsensitive)
    } else if (settings.sortBy == SortBy.FileSize) {
      fr1.compareBySize(fr2, settings.sortCaseInsensitive)
    } else if (settings.sortBy == SortBy.FileType) {
      fr1.compareByType(fr2, settings.sortCaseInsensitive)
    } else if (settings.sortBy == SortBy.LastMod) {
      fr1.compareByLastMod(fr2, settings.sortCaseInsensitive)
    } else {
      fr1.compareByPath(fr2, settings.sortCaseInsensitive)
    }
  }

  def sortFileResults(fileResults: Seq[FileResult]): Seq[FileResult] = {
    val sortedFileResults =
      if (settings.sortBy == SortBy.FileName) {
        fileResults.sortWith((fr1: FileResult, fr2: FileResult) => fr1.compareByName(fr2, settings.sortCaseInsensitive))
      } else if (settings.sortBy == SortBy.FileSize) {
        fileResults.sortWith((fr1: FileResult, fr2: FileResult) => fr1.compareBySize(fr2, settings.sortCaseInsensitive))
      } else if (settings.sortBy == SortBy.FileType) {
        fileResults.sortWith((fr1: FileResult, fr2: FileResult) => fr1.compareByType(fr2, settings.sortCaseInsensitive))
      } else if (settings.sortBy == SortBy.LastMod) {
        fileResults.sortWith((fr1: FileResult, fr2: FileResult) => fr1.compareByLastMod(fr2, settings.sortCaseInsensitive))
      } else {
        fileResults.sortWith((fr1: FileResult, fr2: FileResult) => fr1.compareByPath(fr2, settings.sortCaseInsensitive))
      }
    if (settings.sortDescending) {
      sortedFileResults.reverse
    } else {
      sortedFileResults
    }
  }

  def find(): Seq[FileResult] = {
    val fileResults = mutable.ArrayBuffer.empty[FileResult]
    settings.paths.foreach { path =>
//      val path = Paths.get(p)
      if (Files.isDirectory(path)) {
        // if maxDepth is zero, we can skip since a directory cannot be a result
        if (settings.maxDepth != 0) {
          if (isMatchingDir(path)) {
            if (settings.recursive) {
              fileResults ++= getFileResults(path)
            } else {
              fileResults ++= getDirFileResults(path)
            }
          } else {
            throw new FindException("Startpath does not match find settings")
          }
        }
      } else if (Files.isRegularFile(path)) {
        // if minDepth > zero, we can skip since the file is at depth zero
        if (settings.minDepth <= 0) {
          filterToFileResult(path) match {
            case Some(findFile) =>
              fileResults += findFile
            case None =>
              throw new FindException("Startpath does not match find settings")
          }
        }
      } else {
        throw new FindException("Startpath is not a findable file type")
      }
    }
    sortFileResults(Seq.empty[FileResult] ++ fileResults)
  }
}

object Finder {

  def compareOptionLocalDateTimes(d1: Option[LocalDateTime], d2: Option[LocalDateTime]): Int = {
    if (d1.isEmpty || d2.isEmpty) {
      0
    } else {
      d1.get.compareTo(d2.get)
    }
  }

  val settingsTests: Seq[FindSettings => Option[String]] = Seq[FindSettings => Option[String]](
    ss => if (ss.paths.nonEmpty) None else Some("Startpath not defined"),
    ss => if (ss.paths.forall { p => Files.exists(p) }) None else Some("Startpath not found"),
    ss => if (ss.paths.forall { p => Files.isReadable(p) }) None else Some("Startpath not readable"),
    ss => if (ss.maxDepth > -1 && ss.minDepth > ss.maxDepth) Some("Invalid range for mindepth and maxdepth") else None,
    ss => if (compareOptionLocalDateTimes(ss.maxLastMod, ss.minLastMod) < 0) Some("Invalid range for minlastmod and maxlastmod") else None,
    ss => if (ss.maxSize > 0 && ss.minSize > ss.maxSize) Some("Invalid range for minsize and maxsize") else None,
  )

  def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  def filterByPatterns(s: String, inPatterns: Set[Regex], outPatterns: Set[Regex]): Boolean = {
    ((inPatterns.isEmpty || matchesAnyPattern(s, inPatterns))
      &&
      (outPatterns.isEmpty || !matchesAnyPattern(s, outPatterns)))
  }
}

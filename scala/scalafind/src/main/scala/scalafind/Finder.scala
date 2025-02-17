package scalafind

import scalafind.FileType.FileType
import scalafind.FileUtil.{getExtension, isHidden}

import java.io.IOException
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.nio.file.{Files, Path}
import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.collection.mutable
import scala.util.matching.Regex

class Finder (settings: FindSettings) {
  import Finder.*

  private def validateSettings(): Unit = {
    settingsTests.foreach { t =>
      val res = t(settings)
      if (res.isDefined) {
        throw new FindException(res.get)
      }
    }
  }
  validateSettings()

  def isMatchingDir(path: Path): Boolean = {
    // null or empty path is a match
    if (path == null || path.toString.isEmpty) {
      true
    } else {
      val pathElems = FileUtil.splitPath(path)
      if (!settings.includeHidden && pathElems.exists(p => isHidden(p))) {
        false
      } else {
        filterByPatterns(path.toString, settings.inDirPatterns, settings.outDirPatterns)
      }
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

  private def getFileSizeAndLastMod(p: Path): (Long, Option[FileTime]) = {
    if (settings.needLastMod || settings.needSize) {
      try {
        val stat = Files.readAttributes(p, classOf[BasicFileAttributes])
        val size = if (settings.needSize) stat.size() else 0L
        val lastMod = if (settings.needLastMod) Some(stat.lastModifiedTime()) else None
        (size, lastMod)
      } catch {
        case _: IOException => (0L, None)
      }
    } else {
      (0L, None)
    }
  }

  def filterToFileResult(p: Path): Option[FileResult] = {
    if (!settings.includeHidden && Files.isHidden(p)) {
      None
    } else {
      val fileType = FileTypes.getFileType(p)
      if (fileType == FileType.Archive && !settings.includeArchives && !settings.archivesOnly) {
        None
      } else {
        val (fileSize, lastMod) = getFileSizeAndLastMod(p)
        val fileResult = new FileResult(p, fileType, fileSize, lastMod)
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
  }

  private def sortFileResults(fileResults: Seq[FileResult]): Seq[FileResult] = {
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

  private final def recFindPath(filePath: Path, minDepth: Int, maxDepth: Int, currentDepth: Int): Seq[FileResult] = {
    val recurse =
      if (currentDepth == maxDepth) {
        RecursionType.NoRecurse
      } else if (maxDepth > -1 && currentDepth > maxDepth) {
        RecursionType.Skip
      } else {
        RecursionType.Recurse
      }
    if (recurse == RecursionType.Skip) {
      Seq.empty[FileResult]
    } else {
      val pathDirs = mutable.ArrayBuffer.empty[Path]
      val pathResults = mutable.ArrayBuffer.empty[FileResult]
      try {
        val pathContents: java.nio.file.DirectoryStream[Path] = Files.newDirectoryStream(filePath)
        val iterator = pathContents.iterator()
        while (iterator.hasNext) {
          val path = iterator.next
          if (!Files.isSymbolicLink(path) || settings.followSymlinks) {
            if (Files.isDirectory(path) && recurse == RecursionType.Recurse && isMatchingDir(path)) {
              pathDirs += path
            } else {
              if (Files.isRegularFile(path) && (minDepth < 0 || currentDepth >= minDepth)) {
                val optFileResult: Option[FileResult] = filterToFileResult(path)
                optFileResult.foreach(pathResults += _)
              }
            }
          }
        }
        pathResults ++= pathDirs.flatMap(recFindPath(_, minDepth, maxDepth, currentDepth + 1))
      } catch {
        case e: IOException =>
          e.printStackTrace()
      }
      Seq.empty ++ pathResults
    }
  }

  private final def findPath(filePath: Path): Seq[FileResult] = {
    val fp = if (Files.exists(filePath)) filePath else FileUtil.expandPath(filePath)
    if (Files.isDirectory(fp)) {
      if (settings.maxDepth == 0) {
        Seq.empty[FileResult]
      } else {
        if (isMatchingDir(fp)) {
          val maxDepth = if (settings.recursive) settings.maxDepth else 1
          recFindPath(fp, settings.minDepth, maxDepth, 1)
        } else {
          throw new FindException("Startpath does not match find settings")
        }
      }
    } else {
      if (settings.minDepth > 0) {
        Seq.empty[FileResult]
      } else {
        filterToFileResult(fp) match {
          case Some(fileResult) =>
            Seq(fileResult)
          case None =>
            throw new FindException("Startpath does not match find settings")
        }
      }
    }
  }

  def find(): Seq[FileResult] = {
    val fileResults = mutable.ArrayBuffer.empty[FileResult]
    settings.paths.foreach { path =>
      fileResults ++= findPath(path)
    }
    sortFileResults(Seq.empty[FileResult] ++ fileResults)
  }
}

object Finder {
  private def compareOptionLocalDateTimes(d1: Option[LocalDateTime], d2: Option[LocalDateTime]): Int = {
    if (d1.isEmpty || d2.isEmpty) {
      0
    } else {
      d1.get.compareTo(d2.get)
    }
  }

  private val settingsTests: Seq[FindSettings => Option[String]] = Seq[FindSettings => Option[String]](
    ss => if (ss.paths.nonEmpty) None else Some("Startpath not defined"),
    ss => if (ss.paths.forall { p => Files.exists(p) || Files.exists(FileUtil.expandPath(p)) }) None else Some("Startpath not found"),
    ss => if (ss.paths.forall { p => Files.isReadable(p) || Files.isReadable(FileUtil.expandPath(p)) }) None else Some("Startpath not readable"),
    ss => if (ss.maxDepth > -1 && ss.minDepth > ss.maxDepth) Some("Invalid range for mindepth and maxdepth") else None,
    ss => if (compareOptionLocalDateTimes(ss.maxLastMod, ss.minLastMod) < 0) Some("Invalid range for minlastmod and maxlastmod") else None,
    ss => if (ss.maxSize > 0 && ss.minSize > ss.maxSize) Some("Invalid range for minsize and maxsize") else None,
  )

  private def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  private def filterByPatterns(s: String, inPatterns: Set[Regex], outPatterns: Set[Regex]): Boolean = {
    (inPatterns.isEmpty || matchesAnyPattern(s, inPatterns))
      &&
      (outPatterns.isEmpty || !matchesAnyPattern(s, outPatterns))
  }
}

object RecursionType extends Enumeration {
  type RecursionType = Value
  val Skip, NoRecurse, Recurse = Value
}

package scalafind

import scalafind.FileType.FileType
import scalafind.FileUtil.{getExtension, isHiddenName, isHiddenPath}
import scalafind.FindError.{INVALID_RANGE_MINDEPTH_MAXDEPTH, INVALID_RANGE_MINLASTMOD_MAXLASTMOD, INVALID_RANGE_MINSIZE_MAXSIZE, STARTPATH_DOES_NOT_MATCH, STARTPATH_NOT_DEFINED, STARTPATH_NOT_FOUND, STARTPATH_NOT_READABLE}

import java.io.IOException
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.nio.file.{Files, Path}
import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.collection.mutable
import scala.util.matching.Regex

class Finder (settings: FindSettings) {
  import Finder.*
  
  val fileTypes = new FileTypes()

  private def validateSettings(): Unit = {
    settingsTests.foreach { t =>
      val res = t(settings)
      if (res.isDefined) {
        throw new FindException(res.get)
      }
    }
  }
  validateSettings()

  def filterDirByHidden(path: Path): Boolean = {
    // null or empty path is a match
    if (path == null || path.toString.isEmpty) {
      true
    } else {
      if (!settings.includeHidden && isHiddenPath(path)) {
        false
      } else {
        true
      }
    }
  }

  def filterDirByInPatterns(path: Path): Boolean = {
    // null or empty path is a match
    if (path == null || path.toString.isEmpty) {
      true
    } else {
      filterByInPatterns(path.toString, settings.inDirPatterns)
    }
  }

  def filterDirByOutPatterns(path: Path): Boolean = {
    // null or empty path is a match
    if (path == null || path.toString.isEmpty) {
      true
    } else {
      filterByOutPatterns(path.toString, settings.outDirPatterns)
    }
  }

  def isMatchingDir(path: Path): Boolean = {
    // null or empty path is a match
    if (path == null || path.toString.isEmpty) {
      true
    } else {
      filterDirByHidden(path) && filterDirByInPatterns(path) && filterDirByOutPatterns(path)
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
    if (!isMatchingDir(p.getParent)) {
      None
    } else if (!settings.includeHidden && isHiddenName(p.getFileName.toString)) {
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
            if (Files.isDirectory(path) && recurse == RecursionType.Recurse && filterDirByHidden(path) && filterDirByOutPatterns(path)) {
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
        if (filterDirByHidden(fp) && filterDirByOutPatterns(fp)) {
          val maxDepth = if (settings.recursive) settings.maxDepth else 1
          recFindPath(fp, settings.minDepth, maxDepth, 1)
        } else {
          throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
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
            throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
        }
      }
    }
  }

  def find(): Seq[FileResult] = {
    val fileResults = mutable.ArrayBuffer.empty[FileResult]
    settings.paths.foreach { path =>
      fileResults ++= findPath(path)
    }
    if (fileResults.size > 1) {
      val fileResultSorter = new FileResultSorter(settings)
      fileResultSorter.sort(Seq.empty[FileResult] ++ fileResults)
    } else {
      Seq.empty[FileResult] ++ fileResults
    }
  }

  private def getMatchingDirs(fileResults: Seq[FileResult]): Seq[Path] = {
    fileResults
      .map(fr => FileUtil.pathOrCurrent(fr.path))
      .distinct
      .sortWith(_.toString < _.toString)
  }

  def printMatchingDirs(fileResults: Seq[FileResult], formatter: FileResultFormatter): Unit = {
    val dirs = getMatchingDirs(fileResults)
    if (dirs.nonEmpty) {
      Common.log("\nMatching directories (%d):".format(dirs.length))
      dirs.foreach(d => Common.log(formatter.formatDirPath(d)))
    } else {
      Common.log("\nMatching directories: 0")
    }
  }

  def printMatchingFiles(fileResults: Seq[FileResult], formatter: FileResultFormatter): Unit = {
    if (fileResults.nonEmpty) {
      Common.log("\nMatching files (%d):".format(fileResults.length))
      fileResults.foreach(fr => Common.log(formatter.formatFileResult(fr)))
    } else {
      Common.log("\nMatching files: 0")
    }
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
    ss => if (ss.paths.nonEmpty) None else Some(STARTPATH_NOT_DEFINED.toString),
    ss => if (ss.paths.forall { p => Files.exists(p) || Files.exists(FileUtil.expandPath(p)) }) None else Some(STARTPATH_NOT_FOUND.toString),
    ss => if (ss.paths.forall { p => Files.isReadable(p) || Files.isReadable(FileUtil.expandPath(p)) }) None else Some(STARTPATH_NOT_READABLE.toString),
    ss => if (ss.maxDepth > -1 && ss.minDepth > ss.maxDepth) Some(INVALID_RANGE_MINDEPTH_MAXDEPTH.toString) else None,
    ss => if (compareOptionLocalDateTimes(ss.maxLastMod, ss.minLastMod) < 0) Some(INVALID_RANGE_MINLASTMOD_MAXLASTMOD.toString) else None,
    ss => if (ss.maxSize > 0 && ss.minSize > ss.maxSize) Some(INVALID_RANGE_MINSIZE_MAXSIZE.toString) else None,
  )

  private def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  private def filterByInPatterns(s: String, inPatterns: Set[Regex]): Boolean = {
    inPatterns.isEmpty || matchesAnyPattern(s, inPatterns)
  }

  private def filterByOutPatterns(s: String, outPatterns: Set[Regex]): Boolean = {
    outPatterns.isEmpty || !matchesAnyPattern(s, outPatterns)
  }

  private def filterByPatterns(s: String, inPatterns: Set[Regex], outPatterns: Set[Regex]): Boolean = {
    filterByInPatterns(s, inPatterns) && filterByOutPatterns(s, outPatterns)
  }
}

object RecursionType extends Enumeration {
  type RecursionType = Value
  val Skip, NoRecurse, Recurse = Value
}

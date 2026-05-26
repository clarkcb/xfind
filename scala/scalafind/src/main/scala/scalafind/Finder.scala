package scalafind

import scalafind.FileType.FileType
import scalafind.FileUtil.getExtension
import scalafind.FindError.{INVALID_RANGE_MINDEPTH_MAXDEPTH, INVALID_RANGE_MINLASTMOD_MAXLASTMOD,
  INVALID_RANGE_MINSIZE_MAXSIZE, STARTPATH_DOES_NOT_MATCH, STARTPATH_NOT_DEFINED,
  STARTPATH_NOT_FOUND, STARTPATH_NOT_READABLE}

import java.io.IOException
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.nio.file.{Files, Path}
import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.collection.mutable
import scala.util.matching.Regex

class Finder (settings: FindSettings) {
  import Finder.*

  private def validateSettings(): Unit = {
    if (settings.paths.isEmpty) {
      throw new FindException(STARTPATH_NOT_DEFINED.toString)
    }
    settings.paths.foreach(path =>
      val p =
        if (Files.exists(path)) {
          path
        } else {
          val expandedPath = FileUtil.expandPath(path)
          if (Files.exists(expandedPath)) {
            expandedPath
          } else {
            throw new FindException(STARTPATH_NOT_FOUND.toString)
          }
        }
      if (!Files.isReadable(p)) {
        throw new FindException(STARTPATH_NOT_READABLE.toString)
      }
      if (Files.isSymbolicLink(p)) {
        if (!settings.followSymlinks) {
          throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
        }
      } else if (Files.isDirectory(p)) {
        if (!isTraversableDirPath(p)) {
          throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
        }
      } else if (Files.isRegularFile(p)) {
        if (filterToFileResult(p).isEmpty) {
          throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
        }
      } else {
        // TODO: start path is unknown/invalid type
        throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
      }
    )
    if (settings.maxDepth > -1 && settings.minDepth > settings.maxDepth) {
      throw new FindException(INVALID_RANGE_MINDEPTH_MAXDEPTH.toString)
    }
    if (compareOptionLocalDateTimes(settings.maxLastMod, settings.minLastMod) < 0) {
      throw new FindException(INVALID_RANGE_MINLASTMOD_MAXLASTMOD.toString)
    }
    if (settings.maxSize > 0 && settings.minSize > settings.maxSize) {
      throw new FindException(INVALID_RANGE_MINSIZE_MAXSIZE.toString)
    }
  }
  validateSettings()

  private def isMatchingPathBySymlink(path: Path): Boolean = {
    settings.followSymlinks || !Files.isSymbolicLink(path)
  }

  private def isMatchingDirPathByHidden(dirPath: Path): Boolean = {
    settings.includeHidden || !FileUtil.isHiddenPath(dirPath)
  }

  private def isMatchingDirPathByInPatterns(dirPath: Path): Boolean = {
    emptyOrAnyMatchesAnyPattern(FileUtil.splitPath(dirPath), settings.inDirPatterns)
  }

  private def isMatchingDirPathByOutPatterns(dirPath: Path): Boolean = {
    emptyOrNotMatchesAnyPattern(dirPath.toString, settings.outDirPatterns)
  }

  def isTraversableDirPath(dirPath: Path): Boolean = {
    isMatchingPathBySymlink(dirPath)
      && isMatchingDirPathByHidden(dirPath)
      && isMatchingDirPathByOutPatterns(dirPath)
  }

  def isMatchingDirPath(dirPath: Path): Boolean = {
    isMatchingPathBySymlink(dirPath)
      && isMatchingDirPathByHidden(dirPath)
      && isMatchingDirPathByInPatterns(dirPath)
      && isMatchingDirPathByOutPatterns(dirPath)
  }

  def isNullOrMatchingDirPath(dirPath: Path): Boolean = {
    dirPath== null || dirPath.toString.isEmpty || isMatchingDirPath(dirPath)
  }

  private def isMatchingFileNameByHidden(fileName: String): Boolean = {
    settings.includeHidden || !FileUtil.isHiddenName(fileName)
  }

  def isMatchingArchiveExtension(ext: String): Boolean = {
    emptyOrMatchesAnyString(ext, settings.inArchiveExtensions)
      && emptyOrNotMatchesAnyString(ext, settings.outArchiveExtensions)
  }

  def isMatchingArchiveExtensionForFilePath(filePath: Path): Boolean = {
    if (settings.inArchiveExtensions.nonEmpty || settings.outArchiveExtensions.nonEmpty) {
      val ext = getExtension(filePath.getFileName.toString)
      isMatchingArchiveExtension(ext)
    } else {
      true
    }
  }

  def isMatchingArchiveFileName(fileName: String): Boolean = {
    emptyOrMatchesAnyPattern(fileName, settings.inArchiveFilePatterns)
      && emptyOrNotMatchesAnyPattern(fileName, settings.outArchiveFilePatterns)
  }

  def isMatchingArchiveFileNameForFilePath(filePath: Path): Boolean = {
    if (settings.inArchiveFilePatterns.nonEmpty || settings.outArchiveFilePatterns.nonEmpty) {
      isMatchingArchiveFileName(filePath.getFileName.toString)
    } else {
      true
    }
  }

  def isMatchingArchiveFilePath(filePath: Path): Boolean = {
    isMatchingArchiveExtensionForFilePath(filePath)
      && isMatchingArchiveFileNameForFilePath(filePath)
  }

  def isMatchingArchiveFileResult(fr: FileResult): Boolean = {
    isMatchingArchiveFilePath(fr.path)
  }

  private def isMatchingExtension(ext: String): Boolean = {
    emptyOrMatchesAnyString(ext, settings.inExtensions)
      && emptyOrNotMatchesAnyString(ext, settings.outExtensions)
  }

  private def isMatchingExtensionForFilePath(filePath: Path): Boolean = {
    if (settings.inExtensions.nonEmpty || settings.outExtensions.nonEmpty) {
      isMatchingExtension(getExtension(filePath.getFileName.toString))
    } else {
      true
    }
  }

  private def isMatchingFileName(fileName: String): Boolean = {
    emptyOrMatchesAnyPattern(fileName, settings.inFilePatterns)
      && emptyOrNotMatchesAnyPattern(fileName, settings.outFilePatterns)
  }

  private def isMatchingFileNameForFilePath(filePath: Path): Boolean = {
    if (settings.inFilePatterns.nonEmpty || settings.outFilePatterns.nonEmpty) {
      isMatchingFileName(filePath.getFileName.toString)
    } else {
      true
    }
  }

  private def isMatchingFilePath(filePath: Path): Boolean = {
    // We assume that isNullOrMatchingDirPath(filePath.getParent()) has already been called
    isMatchingExtensionForFilePath(filePath)
      && isMatchingFileNameForFilePath(filePath)
  }

  private def isMatchingFileType(fileType: FileType): Boolean = {
    emptyOrMatchesAnyFileType(fileType, settings.inFileTypes)
      && emptyOrNotMatchesAnyFileType(fileType, settings.outFileTypes)
  }

  private def isMatchingSize(fileSize: Long): Boolean = {
    (settings.maxSize <= 0 || fileSize <= settings.maxSize)
      && (settings.minSize <= 0 || fileSize >= settings.minSize)
  }

  private def isMatchingLastMod(lastMod: Option[Instant]): Boolean = {
    // TODO: lastMod is not needed if maxLastMod and minLastMod are not set, so can avoid getting last mod time for files if not needed
    (settings.maxLastMod, settings.minLastMod) match {
      case (Some(max), Some(min)) =>
        lastMod.nonEmpty &&
        lastMod.get.compareTo(max.toInstant(ZoneOffset.UTC)) <= 0 &&
          lastMod.get.compareTo(min.toInstant(ZoneOffset.UTC)) >= 0
      case (Some(max), None) =>
        lastMod.nonEmpty &&
        lastMod.get.compareTo(max.toInstant(ZoneOffset.UTC)) <= 0
      case (None, Some(min)) =>
        lastMod.nonEmpty &&
        lastMod.get.compareTo(min.toInstant(ZoneOffset.UTC)) >= 0
      case (None, None) =>
        true
    }
  }

  def isMatchingFileResult(fr: FileResult): Boolean = {
    isMatchingFilePath(fr.path)
      && isMatchingFileType(fr.fileType)
      && isMatchingSize(fr.fileSize)
      && isMatchingLastMod(fr.lastMod.map(_.toInstant))
  }

  private def getFileSizeAndLastMod(filePath: Path): (Long, Option[FileTime]) = {
    if (settings.needLastMod || settings.needSize) {
      try {
        val stat = Files.readAttributes(filePath, classOf[BasicFileAttributes])
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

  private def filterArchiveFilePathToFileResult(filePath: Path, fileType: FileType): Option[FileResult] = {
    if (!settings.includeArchives && !settings.archivesOnly) {
      None
    } else if (!isMatchingArchiveFilePath(filePath)) {
      None
    } else {
      Some(new FileResult(filePath, fileType, 0L, None))
    }
  }

  private def filterRegularFilePathToFileResult(filePath: Path, fileType: FileType): Option[FileResult] = {
    if (settings.archivesOnly) {
      None
    } else if (!isMatchingFilePath(filePath) || !isMatchingFileType(fileType)) {
      None
    } else {
      val (fileSize, lastMod) = getFileSizeAndLastMod(filePath)
      if (!isMatchingSize(fileSize) || !isMatchingLastMod(lastMod.map(_.toInstant))) {
        None
      } else {
        Some(new FileResult(filePath, fileType, fileSize, lastMod))
      }
    }
  }

  def filterToFileResult(filePath: Path): Option[FileResult] = {
    if (!isNullOrMatchingDirPath(filePath.getParent)
      || !isMatchingFileNameByHidden(filePath.getFileName.toString)) {
      None
    } else {
      val fileType = FileTypes.getFileType(filePath)
      if (fileType == FileType.Archive) {
        filterArchiveFilePathToFileResult(filePath, fileType)
      } else {
        filterRegularFilePathToFileResult(filePath, fileType)
      }
    }
  }

  private final def recFindPath(path: Path, minDepth: Int, maxDepth: Int, currentDepth: Int): Seq[FileResult] = {
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
        val subPathStream: java.nio.file.DirectoryStream[Path] = Files.newDirectoryStream(path)
        val iterator = subPathStream.iterator()
        while (iterator.hasNext) {
          val subPath = iterator.next
          if (!Files.isSymbolicLink(subPath) || settings.followSymlinks) {
            if (Files.isDirectory(subPath) && recurse == RecursionType.Recurse && isTraversableDirPath(subPath)) {
              pathDirs += subPath
            } else {
              if (Files.isRegularFile(subPath) && (minDepth < 0 || currentDepth >= minDepth)) {
                val optFileResult: Option[FileResult] = filterToFileResult(subPath)
                optFileResult.foreach(pathResults += _)
              }
            }
          }
        }
      } catch {
        case e: IOException =>
          e.printStackTrace()
      }
      Seq.empty ++ pathDirs.flatMap(recFindPath(_, minDepth, maxDepth, currentDepth + 1))
    }
  }

  private final def findPath(path: Path): Seq[FileResult] = {
    val p =
      if (Files.exists(path)) {
        path
      } else {
        val expandedPath = FileUtil.expandPath(path)
        if (Files.exists(expandedPath)) {
          expandedPath
        } else {
          throw new FindException(STARTPATH_NOT_FOUND.toString)
        }
      }
    if (Files.isSymbolicLink(p) && !settings.followSymlinks) {
      throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
    }
    if (Files.isDirectory(p)) {
      if (settings.maxDepth == 0) {
        Seq.empty[FileResult]
      } else {
        if (isTraversableDirPath(p)) {
          val maxDepth = if (settings.recursive) settings.maxDepth else 1
          recFindPath(p, settings.minDepth, maxDepth, 1)
        } else {
          throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
        }
      }
    } else if (Files.isRegularFile(p)) {
      if (settings.minDepth > 0) {
        Seq.empty[FileResult]
      } else {
        filterToFileResult(p) match {
          case Some(fileResult) =>
            Seq(fileResult)
          case None =>
            throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
        }
      }
    } else {
      throw new FindException(STARTPATH_DOES_NOT_MATCH.toString)
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
}

object Finder {
  private def compareOptionLocalDateTimes(d1: Option[LocalDateTime], d2: Option[LocalDateTime]): Int = {
    if (d1.isEmpty || d2.isEmpty) {
      0
    } else {
      d1.get.compareTo(d2.get)
    }
  }

  private def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  private def anyMatchesAnyPattern(strings: Iterable[String], patterns: Set[Regex]): Boolean = {
    strings exists (matchesAnyPattern(_, patterns))
  }

  private def emptyOrMatchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns.isEmpty || patterns.exists (_.findFirstMatchIn(s).isDefined)
  }

  private def emptyOrNotMatchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns.isEmpty || !patterns.exists (_.findFirstMatchIn(s).isDefined)
  }

  private def emptyOrAnyMatchesAnyPattern(strings: Iterable[String], patterns: Set[Regex]): Boolean = {
    patterns.isEmpty || anyMatchesAnyPattern(strings, patterns)
  }

  private def emptyOrNotAnyMatchesAnyPattern(strings: Iterable[String], patterns: Set[Regex]): Boolean = {
    patterns.isEmpty || !anyMatchesAnyPattern(strings, patterns)
  }

  private def emptyOrMatchesAnyString(s: String, stringSet: Set[String]): Boolean = {
    stringSet.isEmpty || stringSet.contains(s)
  }

  private def emptyOrNotMatchesAnyString(s: String, stringSet: Set[String]): Boolean = {
    stringSet.isEmpty || !stringSet.contains(s)
  }

  private def emptyOrMatchesAnyFileType(fileType: FileType, fileTypeSet: Set[FileType]): Boolean = {
    fileTypeSet.isEmpty || fileTypeSet.contains(fileType)
  }

  private def emptyOrNotMatchesAnyFileType(fileType: FileType, fileTypeSet: Set[FileType]): Boolean = {
    fileTypeSet.isEmpty || !fileTypeSet.contains(fileType)
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

object RecursionType extends Enumeration {
  type RecursionType = Value
  val Skip, NoRecurse, Recurse = Value
}

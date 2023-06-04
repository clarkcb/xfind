package scalafind

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import scalafind.Common.log
import scalafind.FileType.FileType
import scalafind.FileUtil.{getExtension, isHidden}
import java.io.IOException
import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.BasicFileAttributes
import java.time.ZoneOffset
import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
import java.nio.charset.Charset
import java.util.zip.{GZIPInputStream, ZipFile}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Finder (settings: FindSettings) {
  import FileUtil.splitPath
  import Finder._

  private def validateSettings(): Unit = {
    settingsTests.foreach { t =>
      t(settings) match {
        case Some(err) => throw new FindException(err)
        case _ =>
      }
    }
  }
  validateSettings()

  def isMatchingDir(p: Path): Boolean = {
    val pathElems = splitPath(p.toString)
    if (settings.excludeHidden && pathElems.exists(p => isHidden(p))) {
      false
    } else {
      filterByPatterns(p.toString, settings.inDirPatterns, settings.outDirPatterns)
    }
  }

  private def matchesExtension(fr: FileResult, inExtensions: Set[String], outExtensions: Set[String]): Boolean = {
    if (inExtensions.nonEmpty || outExtensions.nonEmpty) {
      val ext = getExtension(fr.path.getFileName().toString())
      (inExtensions.isEmpty || inExtensions.contains(ext))
        && (outExtensions.isEmpty || !outExtensions.contains(ext))
    } else {
      true
    }
  }

  private def matchesPattern(fr: FileResult, inFilePatterns: Set[Regex], outFilePatterns: Set[Regex]): Boolean = {
    if (inFilePatterns.nonEmpty || outFilePatterns.nonEmpty) {
      val fileName = fr.path.getFileName().toString()
      (inFilePatterns.isEmpty || matchesAnyPattern(fileName, inFilePatterns))
        && (outFilePatterns.isEmpty || !matchesAnyPattern(fileName, outFilePatterns))
    } else {
      true
    }
  }

  private def matchesFileType(fr: FileResult): Boolean = {
    (settings.inFileTypes.isEmpty || settings.inFileTypes.contains(fr.fileType))
      && (settings.outFileTypes.isEmpty || !settings.outFileTypes.contains(fr.fileType))
  }

  private def matchesStat(fr: FileResult): Boolean = {
    (fr.stat: Option[BasicFileAttributes]) match {
      case Some(st) =>
        (settings.maxLastMod.isEmpty
          || st.lastModifiedTime().toInstant.compareTo(settings.maxLastMod.get.toInstant(ZoneOffset.UTC)) <= 0)
        && (settings.minLastMod.isEmpty
          || st.lastModifiedTime().toInstant.compareTo(settings.minLastMod.get.toInstant(ZoneOffset.UTC)) >= 0)
        && (settings.maxSize == 0
          || st.size.compareTo(settings.maxSize.toLong) <= 0)
        && (settings.minSize == 0
          || st.size.compareTo(settings.minSize.toLong) >= 0)
      case _ => true
    }
  }

  def isMatchingFileResult(fr: FileResult): Boolean = {
    matchesExtension(fr, settings.inExtensions, settings.outExtensions)
      && matchesPattern(fr, settings.inFilePatterns, settings.outFilePatterns)
      && matchesFileType(fr)
      && matchesStat(fr)
  }

  def isMatchingArchiveFileResult(fr: FileResult): Boolean = {
    matchesExtension(fr, settings.inArchiveExtensions, settings.outArchiveExtensions)
      && matchesPattern(fr, settings.inArchiveFilePatterns, settings.outArchiveFilePatterns)
//      && matchesStat(fr)
  }

  def filterToFileResult(p: Path): Option[FileResult] = {
    if (settings.excludeHidden && Files.isHidden(p)) {
      None
    } else {
      val stat: Option[BasicFileAttributes] =
        if (settings.needStat) {
          try {
            Some(Files.readAttributes(p, classOf[BasicFileAttributes]))
          } catch {
            case _: IOException => None
          }
        } else {
          None
        }
      val fileResult = new FileResult(p, FileTypes.getFileType(p.getFileName.toString), stat)
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
    Files.list(startPath)
      .filter(!Files.isDirectory(_))
      .map(filterToFileResult)
      .filter(_.nonEmpty)
      .iterator().asScala.flatten.toSeq
  }

  final def getFileResults(startPath: Path): Seq[FileResult] = {
    getDirFileResults(startPath) ++
      Files.list(startPath)
        .filter(Files.isDirectory(_))
        .filter(isMatchingDir)
        .iterator()
        .asScala
        .toSeq
        .flatMap(getFileResults)
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
    settings.paths.foreach { p =>
      val path = Paths.get(p)
      if (Files.isDirectory(path)) {
        if (isMatchingDir(path)) {
          if (settings.recursive) {
            fileResults ++= getFileResults(path)
          } else {
            fileResults ++= getDirFileResults(path)
          }
        } else {
          throw new FindException("Startpath does not match find settings")
        }
      } else if (Files.isRegularFile(path)) {
        filterToFileResult(path) match {
          case Some(findFile) =>
            fileResults += findFile
          case None =>
            throw new FindException("Startpath does not match find settings")
        }
      } else {
        throw new FindException("Startpath not findable")
      }
    }
    sortFileResults(Seq.empty[FileResult] ++ fileResults)
  }
}

object Finder {

  val settingsTests: Seq[FindSettings => Option[String]] = Seq[FindSettings => Option[String]](
    ss => if (ss.paths.nonEmpty) None else Some("Startpath not defined"),
    ss => if (ss.paths.forall { p => new File(p).exists() }) None else Some("Startpath not found"),
    ss => if (ss.paths.forall { p => new File(p).canRead }) None else Some("Startpath not readable"),
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

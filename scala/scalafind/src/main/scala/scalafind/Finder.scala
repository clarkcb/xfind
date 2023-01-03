package scalafind

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import scalafind.Common.log
import scalafind.FileType.FileType
import scalafind.FileUtil.{getExtension, isHidden}
import java.nio.file.{Files, Path, Paths}

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

  val fileExtTests: Seq[String => Boolean] = {
    val _fileExtTests = mutable.ArrayBuffer.empty[String => Boolean]
    if (settings.inExtensions.nonEmpty) {
      val extTest = (ext: String) => settings.inExtensions.contains(ext)
      _fileExtTests += extTest
    }
    if (settings.outExtensions.nonEmpty) {
      val extTest = (ext: String) => !settings.outExtensions.contains(ext)
      _fileExtTests += extTest
    }
    if (_fileExtTests.isEmpty) {
      val extTest = (ext: String) => true
      _fileExtTests += extTest
    }
    Seq.empty[String => Boolean] ++ _fileExtTests
  }

  val fileNameTests: Seq[String => Boolean] = {
    val _fileNameTests = mutable.ArrayBuffer.empty[String => Boolean]
    if (settings.inFilePatterns.nonEmpty) {
      val nameTest = (fileName: String) => matchesAnyPattern(fileName, settings.inFilePatterns)
      _fileNameTests += nameTest
    }
    if (settings.outFilePatterns.nonEmpty) {
      val nameTest = (fileName: String) => !matchesAnyPattern(fileName, settings.outFilePatterns)
      _fileNameTests += nameTest
    }
    if (_fileNameTests.isEmpty) {
      val nameTest = (fileName: String) => true
      _fileNameTests += nameTest
    }
    Seq.empty[String => Boolean] ++ _fileNameTests
  }

  val fileTypeTests: Seq[FileType => Boolean] = {
    val _fileTypeTests = mutable.ArrayBuffer.empty[FileType => Boolean]
    if (settings.inFileTypes.nonEmpty) {
      val typeTest = (fileType: FileType) => settings.inFileTypes.contains(fileType)
      _fileTypeTests += typeTest
    }
    if (settings.outFileTypes.nonEmpty) {
      val typeTest = (fileType: FileType) => !settings.outFileTypes.contains(fileType)
      _fileTypeTests += typeTest
    }
    if (_fileTypeTests.isEmpty) {
      val typeTest = (fileType: FileType) => true
      _fileTypeTests += typeTest
    }
    Seq.empty[FileType => Boolean] ++ _fileTypeTests
  }

  val archiveFileExtTests: Seq[String => Boolean] = {
    val _archiveFileExtTests = mutable.ArrayBuffer.empty[String => Boolean]
    if (settings.inArchiveExtensions.nonEmpty) {
      val extTest = (ext: String) => settings.inArchiveExtensions.contains(ext)
      _archiveFileExtTests += extTest
    }
    if (settings.outArchiveExtensions.nonEmpty) {
      val extTest = (ext: String) => !settings.outArchiveExtensions.contains(ext)
      _archiveFileExtTests += extTest
    }
    if (_archiveFileExtTests.isEmpty) {
      val extTest = (ext: String) => true
      _archiveFileExtTests += extTest
    }
    Seq.empty[String => Boolean] ++ _archiveFileExtTests
  }

  val archiveFileNameTests: Seq[String => Boolean] = {
    val _archiveFileNameTests = mutable.ArrayBuffer.empty[String => Boolean]
    if (settings.inArchiveFilePatterns.nonEmpty) {
      val nameTest = (fileName: String) => matchesAnyPattern(fileName, settings.inArchiveFilePatterns)
      _archiveFileNameTests += nameTest
    }
    if (settings.outArchiveFilePatterns.nonEmpty) {
      val nameTest = (fileName: String) => !matchesAnyPattern(fileName, settings.outArchiveFilePatterns)
      _archiveFileNameTests += nameTest
    }
    if (_archiveFileNameTests.isEmpty) {
      val nameTest = (fileName: String) => true
      _archiveFileNameTests += nameTest
    }
    Seq.empty[String => Boolean] ++ _archiveFileNameTests
  }

  def validateSettings(): Unit = {
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

  def isMatchingFile(fileResult: FileResult): Boolean = {
    isMatchingFile(fileResult.path.getFileName().toString(), fileResult.fileType)
  }

  def isMatchingFile(fileName: String, fileType: FileType): Boolean = {
    (if (fileExtTests.nonEmpty) {
      val ext = getExtension(fileName)
      fileExtTests.forall(t => t(ext))
    } else {
      true
    }) &&
      fileNameTests.forall(t => t(fileName)) &&
      fileTypeTests.forall(t => t(fileType))
  }

  def isMatchingArchiveFile(fileName: String): Boolean = {
    val ext = getExtension(fileName)
    archiveFileExtTests.forall(t => t(ext)) &&
      archiveFileNameTests.forall(t => t(fileName))
  }

  def filterToFileResult(f: Path): Option[FileResult] = {
    if (settings.excludeHidden && Files.isHidden(f)) {
      None
    } else {
      val fileResult = new FileResult(f, FileTypes.getFileType(f.toString))
      fileResult.fileType match {
        // This is commented out to allow unknown files to match in case settings are permissive
        // case FileType.Unknown => None
        case FileType.Archive =>
          if (settings.includeArchives && isMatchingArchiveFile(f.toString)) {
            Some(fileResult)
          } else {
            None
          }
        case _ =>
          if (!settings.archivesOnly && isMatchingFile(fileResult)) {
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

  def sortFileResults(fileResults: Seq[FileResult]): Seq[FileResult] = {
    val sortedFileResults =
      if (settings.sortBy == SortBy.FileName) {
        fileResults.sortWith((fr1: FileResult, fr2: FileResult) => fr1.compareByName(fr2))
      } else if (settings.sortBy == SortBy.FileType) {
        fileResults.sortWith((fr1: FileResult, fr2: FileResult) => fr1.compareByType(fr2))
      } else {
        fileResults.sortWith((fr1: FileResult, fr2: FileResult) => fr1.compareByPath(fr2))
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
  val currentPath = "."
  val tarExtension = "tar"

  val settingsTests: Seq[FindSettings => Option[String]] = Seq[FindSettings => Option[String]](
    ss => if (ss.paths.nonEmpty) None else Some("Startpath not defined"),
    ss => if (ss.paths.forall { p => new File(p).exists() }) None else Some("Startpath not found"),
    ss => if (ss.paths.forall { p => new File(p).canRead }) None else Some("Startpath not readable"),
  )

  def listToString(stringList: Iterable[Any]): String = {
    stringList.mkString("[\"", "\", \"", "\"]")
  }

  def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  def anyMatchesAnyPattern(strings: Seq[String], patterns: Set[Regex]): Boolean = {
    strings exists (matchesAnyPattern(_, patterns))
  }

  def filterByPatterns(s: String, inPatterns: Set[Regex], outPatterns: Set[Regex]): Boolean = {
    ((inPatterns.isEmpty || matchesAnyPattern(s, inPatterns))
      &&
      (outPatterns.isEmpty || !matchesAnyPattern(s, outPatterns)))
  }

  def linesMatch(lines: Seq[String], inPatterns: Set[Regex],
                         outPatterns: Set[Regex]): Boolean = {
    (inPatterns.isEmpty || anyMatchesAnyPattern(lines, inPatterns)) &&
      (outPatterns.isEmpty || !anyMatchesAnyPattern(lines, outPatterns))
  }

  def getLineIndices(contents:String): Seq[(Int,Int)] = {
    val newLineIndices = contents.zipWithIndex.collect { case ('\n',i) => i }
    if (newLineIndices.nonEmpty) {
      val lineIndices = mutable.ArrayBuffer[(Int,Int)]((0,newLineIndices.head))
      lineIndices ++= newLineIndices.map(_ + 1).zip(newLineIndices.tail :+ contents.length)
      lineIndices.toSeq
    } else {
      Seq[(Int,Int)]((0,contents.length-1))
    }
  }
}

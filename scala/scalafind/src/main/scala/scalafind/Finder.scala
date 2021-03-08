package scalafind

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import scalafind.Common.log
import scalafind.FileType.FileType

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
import java.nio.charset.Charset
import java.util.zip.{GZIPInputStream, ZipFile}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

class Finder (settings: FindSettings) {
  import FileUtil._
  import Finder._

  def validateSettings(): Unit = {
    settingsTests.foreach { t =>
      t(settings) match {
        case Some(err) => throw new FindException(err)
        case _ =>
      }
    }
  }
  validateSettings()

  def isFindDir(d: File): Boolean = {
    isFindDir(d.getName)
  }

  def isFindDir(dirName: String): Boolean = {
    val pathElems = splitPath(dirName)
    if (pathElems.exists(p => isHidden(p)) && settings.excludeHidden) {
      false
    } else {
      filterByPatterns(dirName, settings.inDirPatterns, settings.outDirPatterns)
    }
  }

  def isFindFile(sf: FindFile): Boolean = {
    isFindFile(sf.file.getPath, sf.fileType)
  }

  def isFindFile(fileName: String, fileType: FileType): Boolean = {
    val fileNameTests = Seq[String => Boolean](
      fileName => !isHidden(fileName) || !settings.excludeHidden,
      fileName => settings.inExtensions.isEmpty ||
        settings.inExtensions.contains(getExtension(fileName)),
      fileName => settings.outExtensions.isEmpty ||
        !settings.outExtensions.contains(getExtension(fileName)),
    )
    val fileTypeTests = Seq[FileType => Boolean](
      fileType => settings.inFileTypes.isEmpty ||
        settings.inFileTypes.contains(fileType),
      fileType => settings.outFileTypes.isEmpty ||
        !settings.outFileTypes.contains(fileType),
    )

    fileNameTests.forall(t => t(fileName)) &&
      fileTypeTests.forall(t => t(fileType)) &&
      filterByPatterns(fileName, settings.inFilePatterns, settings.outFilePatterns)
  }

  def isArchiveFindFile(fileName: String): Boolean = {
    val fileNameTests = Seq[String => Boolean](
      fileName => !isHidden(fileName) || !settings.excludeHidden,
      fileName => settings.inArchiveExtensions.isEmpty ||
        settings.inArchiveExtensions.contains(getExtension(fileName)),
      fileName => settings.outArchiveExtensions.isEmpty ||
        !settings.outArchiveExtensions.contains(getExtension(fileName)),
    )
    fileNameTests.forall(t => t(fileName)) &&
      filterByPatterns(fileName, settings.inArchiveFilePatterns, settings.outArchiveFilePatterns)
  }

  def filterToFindFile(f: File): Option[FindFile] = {
    val fileType = FileTypes.getFileType(f.getName)
    fileType match {
      // This is commented out to allow unknown files to match in case settings are permissive
      // case FileType.Unknown => None
      case FileType.Archive =>
        if (settings.includeArchives && isArchiveFindFile(f.getName)) {
          Some(new FindFile(f, fileType))
        } else {
          None
        }
      case _ =>
        if (!settings.archivesOnly && isFindFile(f.getName, fileType)) {
          Some(new FindFile(f, fileType))
        } else {
          None
        }
    }
  }

  final def getFindFiles(startPathFile: File): Seq[FindFile] = {
    val files = startPathFile.listFiles
    files.filter(_.isFile).flatMap(filterToFindFile) ++
      files
        .filter(_.isDirectory)
        .filter(_ => settings.recursive)
        .filter(isFindDir)
        .flatMap(getFindFiles)
  }

  def find(): Seq[FindFile] = {
    val findFiles = mutable.ListBuffer.empty[FindFile]
    settings.paths.foreach { p =>
      val pFile = new File(p)
      if (pFile.isDirectory) {
        if (isFindDir(pFile)) {
          findFiles ++= getFindFiles(pFile)
        } else {
          throw new FindException("Startpath does not match find settings")
        }
      } else if (pFile.isFile) {
        filterToFindFile(pFile) match {
          case Some(findFile) =>
            findFiles += findFile
          case None => 
            throw new FindException("Startpath does not match find settings")
        }
      } else {
        throw new FindException("Startpath not findable")
      }
    }
    Seq.empty[FindFile] ++ findFiles
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

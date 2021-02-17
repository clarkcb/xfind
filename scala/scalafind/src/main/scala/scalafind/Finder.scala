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
    try {
      val _ = Charset.forName(settings.textFileEncoding)
    } catch {
      case _: IllegalArgumentException =>
        throw new FindException(s"Invalid encoding: ${settings.textFileEncoding}")
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

  def filterFile(f: File): Boolean = {
    val fileType = FileTypes.getFileType(f.getName)
    fileType match {
      case FileType.Unknown => false
      case FileType.Archive =>
        settings.findArchives && isArchiveFindFile(f.getName)
      case _ =>
        !settings.archivesOnly && isFindFile(f.getName, fileType)
    }
  }

  def filterToFindFile(f: File): Option[FindFile] = {
    val fileType = FileTypes.getFileType(f.getName)
    fileType match {
      case FileType.Unknown => None
      case FileType.Archive =>
        if (settings.findArchives && isArchiveFindFile(f.getName)) {
          Some(new FindFile(f, fileType))
        } else {
          None
        }
      case _ =>
        if (!settings.archivesOnly && isFindFile(f.getPath, fileType)) {
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

  def find(): Seq[FindResult] = {
    val startPathFile = new File(settings.startPath.get)
    if (startPathFile.isDirectory) {
      if (isFindDir(startPathFile)) {
        findPath(startPathFile)
      } else {
        throw new FindException("Startpath does not match find settings")
      }
    } else if (startPathFile.isFile) {
      if (filterFile(startPathFile)) {
        val fileType = FileTypes.getFileType(startPathFile.getName)
        findFile(new FindFile(startPathFile, fileType))
      } else {
        throw new FindException("Startpath does not match find settings")
      }
    } else {
      throw new FindException("Startpath not findable")
    }
  }

  def findPath(startPath: File): Seq[FindResult] = {
    val files: Seq[FindFile] = getFindFiles(startPath)
    if (settings.verbose) {
      val dirs = files.map(f => pathOrCurrent(f.file.getParentFile))
        .map(_.getPath).distinct.sorted
      log("\nDirectories to be found (%d):\n%s".format(dirs.size,
        dirs.mkString("\n")))
      log("\nFiles to be found (%d):\n%s".format(files.size,
        files.mkString("\n")))
      log("\nStarting file find...\n")
    }
    val results: Seq[FindResult] = findFiles(files)
    if (settings.verbose) {
      log("\nFile find complete.\n")
    }
    results
  }

  def findFiles(files: Seq[FindFile]): Seq[FindResult] = {
    var offset = 0
    val batchSize = 1000
    var until = offset + batchSize

    if (files.length > batchSize) {
      val results = mutable.ListBuffer.empty[FindResult]
      while (offset < files.length) {
        results ++= batchFindFiles(files.slice(offset, until))
        offset = math.min(offset + batchSize, files.length)
        until = offset + batchSize
      }
      Seq.empty[FindResult] ++ results

    } else {
      files.flatMap { f =>
        findFile(f)
      }
    }
  }

  def batchFindFiles(files: Seq[FindFile]): Seq[FindResult] = {
    import java.util.concurrent.ConcurrentHashMap

    val findFileResultsMap: ConcurrentHashMap[String, Seq[FindResult]] = new ConcurrentHashMap

    files.map { f =>
      Future {
        findFile(f)
      }.map { results =>
        findFileResultsMap put (f.getPath, results)
      }
    }

    while (findFileResultsMap.size() < files.length) {
      Thread.sleep(500)
    }

    val results = mutable.ListBuffer.empty[FindResult]
    for (k <- findFileResultsMap.asScala.keySet) {
      results ++= findFileResultsMap.get(k)
    }
    Seq.empty[FindResult] ++ results
  }

  def findFile(sf: FindFile): Seq[FindResult] = {
    FileTypes.getFileType(sf.file.getName) match {
      case ft if Set(FileType.Text, FileType.Code, FileType.Xml).contains(ft) =>
        findTextFileSource(sf, Source.fromFile(sf.file, settings.textFileEncoding))
      case FileType.Binary =>
        findBinaryFileSource(sf, Source.fromFile(sf.file, "ISO-8859-1"))
      case FileType.Archive =>
        findArchiveFileSource(sf, Source.fromFile(sf.file))
      case _ =>
        log("Skipping unknown file type: %s".format(sf))
        Seq.empty[FindResult]
    }
  }

  private def findFileSource(sf: FindFile, source: Source): Seq[FindResult] = {
    FileTypes.getFileType(sf.file.getName) match {
      case ft if Set(FileType.Code, FileType.Text, FileType.Xml).contains(ft) =>
        findTextFileSource(sf, source)
      case FileType.Binary =>
        findBinaryFileSource(sf, source)
      case FileType.Archive =>
        findArchiveFileSource(sf, source)
      case _ =>
        log("Skipping unknown file type: %s".format(sf))
        Seq.empty[FindResult]
    }
  }

  private def findTextFileSource(sf: FindFile, source: Source): Seq[FindResult] = {
    if (settings.verbose) {
      log("Finding text file %s".format(sf.getPathWithContainers))
    }
    if (settings.multiLineFind) {
      findTextFileSourceContents(sf, source)
    } else {
      findTextFileSourceLines(sf, source)
    }
  }

  private def findTextFileSourceContents(sf: FindFile, source: Source): Seq[FindResult] = {
    try {
      val contents = source.mkString
      findMultiLineString(contents).map { r =>
        r.copy(file = Some(sf))
      }
    } catch {
      case _: java.nio.charset.MalformedInputException =>
        if (settings.verbose) {
          log(s"Skipping file with unsupported encoding: $sf")
        }
        Seq.empty[FindResult]
    }
  }

  def findMultiLineString(s: String): Seq[FindResult] = {
    settings.findPatterns.flatMap { p =>
      findMultiLineStringForPattern(s, p)
    }.toSeq
  }

  private def findMultiLineStringForPattern(s: String, p: Regex): Seq[FindResult] = {
    val lineIndices: Seq[(Int, Int)] = Finder.getLineIndices(s)
    val results = mutable.ArrayBuffer.empty[FindResult]
    val matches = p.findAllIn(s).matchData
    var stop = false

    def getLinesAfterFromMultiLineString(s: String, startIndex: Int,
                                         lineIndices: Seq[(Int, Int)]): Seq[String] = {
      if (settings.hasLinesAfterToOrUntilPatterns) {
        val matchIndices = (settings.linesAfterToPatterns ++ settings.linesAfterUntilPatterns).map {
          p =>
            p.findFirstMatchIn(s.substring(startIndex)) match {
              case Some(m) => m.start + startIndex
              case None => -1
            }
        }.filter(_ > -1)
        if (matchIndices.nonEmpty) {
          val lines = lineIndices.
            filter(_._1 < matchIndices.min).
            map(li => s.substring(li._1, li._2))
          if (settings.hasLinesAfterUntilPatterns && lines.nonEmpty) {
            lines.init
          } else {
            lines
          }
        } else {
          Seq.empty[String]
        }
      } else if (settings.linesAfter > 0) {
        lineIndices.
          take(settings.linesAfter).
          map(li => s.substring(li._1, li._2))
      } else {
        Seq.empty[String]
      }
    }

    while (matches.hasNext && !stop) {
      val m = matches.next()
      val thisLineIndices: (Int, Int) = lineIndices.filter(_._1 <= m.start).last
      val beforeLineCount = lineIndices.count(_._1 < thisLineIndices._1)
      val line = s.substring(thisLineIndices._1, thisLineIndices._2)
      val linesBefore =
        if (settings.linesBefore > 0) {
          lineIndices.filter(_._1 < thisLineIndices._1).
            takeRight(settings.linesBefore).
            map(li => s.substring(li._1, li._2))
        } else {
          Seq.empty[String]
        }
      val linesAfter = getLinesAfterFromMultiLineString(s, thisLineIndices._2,
        lineIndices.filter(_._1 > thisLineIndices._2))
      if (linesBeforeMatch(linesBefore) && linesAfterMatch(linesAfter)) {
        results += FindResult(
          p,
          None,
          beforeLineCount + 1,
          m.start - thisLineIndices._1 + 1,
          m.end - thisLineIndices._1 + 1,
          Some(line),
          linesBefore,
          linesAfter)
        if (settings.firstMatch) {
          stop = true
        }
      }
    }
    results.toSeq
  }

  private def linesBeforeMatch(linesBefore: Seq[String]): Boolean = {
    if (settings.hasLinesBeforePatterns) {
      linesMatch(linesBefore, settings.inLinesBeforePatterns,
        settings.outLinesBeforePatterns)
    } else {
      true
    }
  }

  private def linesAfterMatch(linesAfter: Seq[String]): Boolean = {
    if (settings.hasLinesAfterPatterns) {
      linesMatch(linesAfter, settings.inLinesAfterPatterns,
        settings.outLinesAfterPatterns)
    } else {
      true
    }
  }

  private def matchLinesAfterToOrUntil(linesAfter: mutable.ListBuffer[String],
                                       lines: Iterator[String]): Boolean = {
    if (settings.hasLinesAfterToOrUntilPatterns) {
      linesAfter.zipWithIndex.foreach { li =>
        if (matchesAnyPattern(li._1, settings.linesAfterToPatterns)) {
          while (li._2 + 1 < linesAfter.size) linesAfter.remove(li._2 + 1)
          return true
        } else if (matchesAnyPattern(li._1, settings.linesAfterUntilPatterns)) {
          while (li._2 < linesAfter.size) linesAfter.remove(li._2)
          return true
        }
      }
      var foundMatch = false
      while (!foundMatch && lines.hasNext) {
        val nextLine = lines.next()
        if (matchesAnyPattern(nextLine, settings.linesAfterToPatterns)) {
          linesAfter += nextLine
          foundMatch = true
        } else if (matchesAnyPattern(nextLine, settings.linesAfterUntilPatterns)) {
          foundMatch = true
        } else {
          linesAfter += nextLine
        }
      }
      foundMatch
    } else {
      true
    }
  }

  private def findTextFileSourceLines(sf: FindFile, source: Source): Seq[FindResult] = {
    try {
      findStringIterator(source.getLines()).map { r =>
        r.copy(file=Some(sf))
      }
    } catch {
      case _: java.nio.charset.MalformedInputException =>
        if (settings.verbose) {
          log(s"Skipping file with unsupported encoding: $sf")
        }
        Seq.empty[FindResult]
    }
  }

  def findStringIterator(lines: Iterator[String]): Seq[FindResult] = {
    val results = mutable.ArrayBuffer.empty[FindResult]
    val linesBefore = new mutable.ListBuffer[String]
    val linesAfter = new mutable.ListBuffer[String]
    var lineNum: Int = 0
    val matchedPatterns = mutable.Set.empty[Regex]
    var stop = false
    while ((lines.hasNext || linesAfter.nonEmpty) && !stop) {
      lineNum += 1
      val line =
        if (linesAfter.nonEmpty) {
          linesAfter.remove(0)
        } else {
          lines.next()
        }
      if (settings.linesAfter > 0) {
        while (linesAfter.length < settings.linesAfter && lines.hasNext)
          linesAfter += lines.next
      }

      val findPatterns =
        if (settings.firstMatch)
          settings.findPatterns.diff(matchedPatterns)
        else settings.findPatterns

      if (findPatterns.isEmpty) {
        stop = true
      }

      findPatterns.foreach { p =>
        val matchIterator: Iterator[Regex.Match] =
          if (settings.firstMatch) p.findFirstMatchIn(line).iterator
          else p.findAllMatchIn(line)

        if (matchIterator.hasNext
          && linesBeforeMatch(linesBefore.toSeq)
          && linesAfterMatch(linesAfter.toSeq)
          && matchLinesAfterToOrUntil(linesAfter, lines)) {

          matchedPatterns.add(p)

          for (m <- matchIterator) {
            results += FindResult(
              p,
              None,
              lineNum,
              m.start + 1,
              m.end + 1,
              Some(line),
              linesBefore.toList,
              linesAfter.toList)
          }
        }
      }

      if (settings.linesBefore > 0) {
        if (linesBefore.length == settings.linesBefore) {
          linesBefore.remove(0, 1)
        }
        if (linesBefore.length < settings.linesBefore) {
          linesBefore += line
        }
      }
    }

    Seq.empty[FindResult] ++ results
  }

  private def findBinaryFileSource(sf: FindFile, source: Source): Seq[FindResult] = {
    if (settings.verbose) {
      log("Finding binary file %s".format(sf.toString))
    }
    val contents = source.mkString
    source.close()
    var results = mutable.ListBuffer[FindResult]()
    for (p <- settings.findPatterns) {
      val matchIterator =
        if (settings.firstMatch) {
          p.findFirstMatchIn(contents)
        } else {
          p.findAllMatchIn(contents)
        }
      for (m <- matchIterator) {
        results += new FindResult(p, Some(sf), 0, m.start + 1, m.end + 1, None)
      }
    }
    Seq.empty[FindResult] ++ results
  }

  private def findArchiveFileSource(sf: FindFile, source: Source): Seq[FindResult] = {
    if (settings.verbose) {
      log("Finding archive file %s".format(sf.toString))
    }
    if (FileTypes.isZipArchiveFile(sf)) {
      findZipFile(sf)
    } else if (FileTypes.isGzArchiveFile(sf)) {
      findGzFile(sf)
    } else if (FileTypes.isBz2ArchiveFile(sf)) {
      findBz2File(sf)
    } else if (FileTypes.isTarArchiveFile(sf)) {
      val tis = new BufferedInputStream(new FileInputStream(sf.getPath))
      findTarFileInputStream(sf, tis)
    } else {
      log("Currently unsupported archive file type: %s (%s)".
        format(getExtension(sf), sf.toString))
      Seq.empty[FindResult]
    }
  }

  private def findZipFile(sf: FindFile): Seq[FindResult] = {
    if (settings.verbose) {
      log("Finding zip file %s".format(sf.toString))
    }
    val zf = new ZipFile(sf.file)
    zf.entries().asScala.filterNot(_.isDirectory)
      .map(e => new File(e.getName))
      .filter { f =>
        Option(f.getParent) match {
          case Some(d) => isFindDir(d)
          case None => true
        }
      }
      .filter(filterFile)
      .flatMap { f =>
        val fileType = FileTypes.getFileType(f.getName)
        if (fileType != FileType.Unknown) {
          val zsf = new FindFile(sf.containers :+ sf.getPath, f, fileType)
          val zis = zf.getInputStream(zf.getEntry(zsf.getPath))
          val entryResults = fileType match {
            case FileType.Archive =>
              findArchiveFileSource(zsf, Source.fromInputStream(zis))
            case _ =>
              findFileSource(zsf, Source.fromInputStream(zis))
          }
          zis.close()
          entryResults
        } else {
          Seq.empty[FindResult]
        }
      }.toSeq
  }

  private def findTarFileInputStream(sf: FindFile, is: InputStream): Seq[FindResult] = {
    if (settings.verbose) {
      log("Finding tar file %s".format(sf.toString))
    }
    val tarResults = mutable.ListBuffer[FindResult]()
    val tis = new TarArchiveInputStream(is)
    var entry: TarArchiveEntry = tis.getNextTarEntry
    while (entry != null) {
      if (!entry.isDirectory) {
        val dirName = new File(entry.getName).getParent
        if (isFindDir(dirName)) {
          val file = new File(entry.getName)
          val fileType = FileTypes.getFileType(file.getName)
          if (fileType != FileType.Unknown) {
            val bytes = new Array[Byte](entry.getSize.toInt)
            val count = tis.read(bytes, 0, entry.getSize.toInt)
            if (count > 0) {
              val tzsf = new FindFile(sf.containers, file, fileType)
              val source = Source.fromBytes(bytes)
              fileType match {
                case FileType.Archive =>
                  tarResults ++= findArchiveFileSource(tzsf, source)
                case _ =>
                  tarResults ++= findFileSource(tzsf, source)
              }
            }
          }
        }
      }
      entry = tis.getNextTarEntry
    }
    Seq.empty[FindResult] ++ tarResults
  }

  private def findGzFile(sf: FindFile): Seq[FindResult] = {
    if (settings.verbose) {
      log("Finding gzip file %s".format(sf.toString))
    }
    val containedFileName = sf.file.getName.split("\\.").init.mkString(currentPath)
    val containedFileType = FileTypes.getFileType(sf.file.getName)
    val containedExtension = getExtension(sf)
    val gzsf = new FindFile(sf.containers :+ sf.getPath, new File(containedFileName),
      containedFileType)
    val gzResults = mutable.ListBuffer[FindResult]()
    val fileType = FileTypes.getFileType(gzsf.file.getName)
    if (containedExtension == tarExtension || isFindFile(gzsf.file.getName, fileType)) {
      val gzis = new GZIPInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (containedExtension == tarExtension) {
        gzResults ++= findTarFileInputStream(gzsf, gzis)
      } else {
        val source = Source.fromInputStream(gzis)
        gzResults ++= findFileSource(gzsf, source)
      }
      gzis.close()
    }
    Seq.empty[FindResult] ++ gzResults
  }

  private def findBz2File(sf: FindFile): Seq[FindResult] = {
    if (settings.verbose) {
      log("Finding bzip2 file %s".format(sf.toString))
    }
    val containedFileName = sf.file.getName.split("\\.").init.mkString(currentPath)
    val containedFileType = FileTypes.getFileType(sf.file.getName)
    val containedExtension = getExtension(sf)
    val bzsf = new FindFile(sf.containers :+ sf.getPath, new File(containedFileName),
      containedFileType)
    val bzResults = mutable.ListBuffer[FindResult]()
    val fileType = FileTypes.getFileType(bzsf.file.getName)
    if (containedExtension == tarExtension || isFindFile(bzsf.file.getName, fileType)) {
      val bzis = new BZip2CompressorInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (containedExtension == tarExtension) {
        bzResults ++= findTarFileInputStream(bzsf, bzis)
      } else {
        val source = Source.fromInputStream(bzis)
        bzResults ++= findFileSource(bzsf, source)
      }
      bzis.close()
    }
    Seq.empty[FindResult] ++ bzResults
  }
}

object Finder {
  val currentPath = "."
  val tarExtension = "tar"

  val settingsTests: Seq[FindSettings => Option[String]] = Seq[FindSettings => Option[String]](
    ss => if (ss.startPath.isDefined && ss.startPath.get.nonEmpty) None else Some("Startpath not defined"),
    ss => if (new File(ss.startPath.get).exists()) None else Some("Startpath not found"),
    ss => if (new File(ss.startPath.get).canRead) None else Some("Startpath not readable"),
    ss => if (ss.findPatterns.nonEmpty) None else Some("No find patterns defined"),
    ss => if (ss.linesAfter >= 0) None else Some("Invalid linesafter"),
    ss => if (ss.linesBefore >= 0) None else Some("Invalid linesbefore"),
    ss => if (ss.maxLineLength >= 0) None else Some("Invalid maxlinelength"),
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

package scalafind

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File
import scala.io.Source

class FinderTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  def getFindSettings: FindSettings = {
    FindSettings(paths = Set("."))
  }

  def getFileLines(file: File): Iterator[String] = {
    val source = Source.fromFile(file)
    val lines = source.getLines()
    source.close()
    lines
  }

  /*************************************************************
   * isMatchingDir tests
   *************************************************************/
  test("testIsMatchingDir_SingleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File(".")))
  }

  test("testIsMatchingDir_DoubleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File("..")))
  }

  test("testIsMatchingDir_IsHidden_False") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(new File(".git")))
  }

  test("testIsMatchingDir_IsHiddenIncludeHidden_True") {
    val settings = getFindSettings.copy(excludeHidden = false)
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File(".git")))
  }

  test("testIsMatchingDir_NoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File("/Users")))
  }

  test("testIsMatchingDir_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File("CsFind")))
  }

  test("testIsMatchingDir_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(new File("CsFind")))
  }

  test("testIsMatchingDir_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(new File("CsFind")))
  }

  test("testIsMatchingDir_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    val dir = new File("CsFind")
    assert(finder.isMatchingDir(dir))
  }

  /*************************************************************
   * isMatchingFile tests
   *************************************************************/
  test("testIsMatchingFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  test("testIsMatchingFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  test("testIsMatchingFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inExtensions = Set("java"))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFile(file))
  }

  test("testIsMatchingFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFile(file))
  }

  test("testIsMatchingFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outExtensions = Set("java"))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  test("testIsMatchingFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FileResult(new File("Finder.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  test("testIsMatchingFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFile(file))
  }

  test("testIsMatchingFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FileResult(new File("Finder.cs"), FileType.Code)
    assert(!finder.isMatchingFile(file))
  }

  test("testIsMatchingFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  /*************************************************************
   * isMatchingArchiveFile tests
   *************************************************************/
  test("testIsMatchingArchiveFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsMatchingArchiveFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsMatchingArchiveFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsMatchingArchiveFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsMatchingArchiveFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsMatchingArchiveFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsMatchingArchiveFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsMatchingArchiveFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsMatchingArchiveFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }

  /*************************************************************
   * filterToFileResult tests
   *************************************************************/
  test("testFilterToFileResult_IsHidden_None") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File(".gitignore")
    assert(finder.filterToFileResult(file).isEmpty)
  }

  test("testFilterToFileResult_IsHiddenIncludeHidden_Some") {
    val settings = getFindSettings.copy(excludeHidden = false)
    val finder = new Finder(settings)
    val file = new File(".hidden.txt")
    println("FileUtil.isHidden(\"%s\"): %s".format(file.getName,
      FileUtil.isHidden(file.getName)))
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_ArchiveNoFindArchives_None") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFileResult(file).isEmpty)
  }

  test("testFilterToFileResult_ArchiveFindArchives_Some") {
    val settings = getFindSettings.copy(includeArchives = true)
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_IsArchiveFindFile_Some") {
    val settings = getFindSettings.copy(includeArchives = true,
      inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_NotIsArchiveFindFile_None") {
    val settings = getFindSettings.copy(outExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFileResult(file).isEmpty)
  }

  test("testFilterToFileResult_ArchiveFileArchivesOnly_Some") {
    val settings = getFindSettings.copy(archivesOnly = true)
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_NoExtensionsNoPatterns_Some") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_IsFindFile_Some") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_NotIsFindFile_None") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterToFileResult(file).isEmpty)
  }

  test("testFilterToFileResult_NonArchiveFileArchivesOnly_None") {
    val settings = getFindSettings.copy(archivesOnly = true)
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterToFileResult(file).isEmpty)
  }
}

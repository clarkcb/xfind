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
   * isFindDir tests
   *************************************************************/
  test("testisFindDir_SingleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isFindDir(new File(".")))
  }

  test("testisFindDir_DoubleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isFindDir(new File("..")))
  }

  test("testisFindDir_IsHidden_False") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(!finder.isFindDir(new File(".git")))
  }

  test("testisFindDir_IsHiddenIncludeHidden_True") {
    val settings = getFindSettings.copy(excludeHidden = false)
    val finder = new Finder(settings)
    assert(finder.isFindDir(new File(".git")))
  }

  test("testisFindDir_NoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isFindDir(new File("/Users")))
  }

  test("testisFindDir_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(finder.isFindDir(new File("CsFind")))
  }

  test("testisFindDir_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(!finder.isFindDir(new File("CsFind")))
  }

  test("testisFindDir_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    assert(!finder.isFindDir(new File("CsFind")))
  }

  test("testisFindDir_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    val dir = new File("CsFind")
    assert(finder.isFindDir(dir))
  }

  /*************************************************************
   * isFindFile tests
   *************************************************************/
  test("testIsFindFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new FindFile(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isFindFile(file))
  }

  test("testIsFindFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new FindFile(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isFindFile(file))
  }

  test("testIsFindFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inExtensions = Set("java"))
    val finder = new Finder(settings)
    val file = new FindFile(new File("FileUtil.cs"), FileType.Code)
    assert(!finder.isFindFile(file))
  }

  test("testIsFindFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new FindFile(new File("FileUtil.cs"), FileType.Code)
    assert(!finder.isFindFile(file))
  }

  test("testIsFindFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outExtensions = Set("java"))
    val finder = new Finder(settings)
    val file = new FindFile(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isFindFile(file))
  }

  test("testIsFindFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FindFile(new File("Finder.cs"), FileType.Code)
    assert(finder.isFindFile(file))
  }

  test("testIsFindFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FindFile(new File("FileUtil.cs"), FileType.Code)
    assert(!finder.isFindFile(file))
  }

  test("testIsFindFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FindFile(new File("Finder.cs"), FileType.Code)
    assert(!finder.isFindFile(file))
  }

  test("testIsFindFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FindFile(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isFindFile(file))
  }

  /*************************************************************
   * isArchiveFindFile tests
   *************************************************************/
  test("testIsArchiveFindFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isArchiveFindFile(file.getName))
  }

  test("testIsArchiveFindFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isArchiveFindFile(file.getName))
  }

  test("testIsArchiveFindFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isArchiveFindFile(file.getName))
  }

  test("testIsArchiveFindFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isArchiveFindFile(file.getName))
  }

  test("testIsArchiveFindFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isArchiveFindFile(file.getName))
  }

  test("testIsArchiveFindFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isArchiveFindFile(file.getName))
  }

  test("testIsArchiveFindFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isArchiveFindFile(file.getName))
  }

  test("testIsArchiveFindFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isArchiveFindFile(file.getName))
  }

  test("testIsArchiveFindFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isArchiveFindFile(file.getName))
  }

  /*************************************************************
   * filterToFindFile tests
   *************************************************************/
  test("testFilterToFindFile_IsHidden_None") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File(".gitignore")
    assert(finder.filterToFindFile(file).isEmpty)
  }

  test("testFilterToFindFile_IsHiddenIncludeHidden_Some") {
    val settings = getFindSettings.copy(excludeHidden = false)
    val finder = new Finder(settings)
    val file = new File(".hidden.txt")
    println("FileUtil.isHidden(\"%s\"): %s".format(file.getName,
      FileUtil.isHidden(file.getName)))
    assert(finder.filterToFindFile(file).nonEmpty)
  }

  test("testFilterToFindFile_ArchiveNoFindArchives_None") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFindFile(file).isEmpty)
  }

  test("testFilterToFindFile_ArchiveFindArchives_Some") {
    val settings = getFindSettings.copy(includeArchives = true)
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFindFile(file).nonEmpty)
  }

  test("testFilterToFindFile_IsArchiveFindFile_Some") {
    val settings = getFindSettings.copy(includeArchives = true,
      inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFindFile(file).nonEmpty)
  }

  test("testFilterToFindFile_NotIsArchiveFindFile_None") {
    val settings = getFindSettings.copy(outExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFindFile(file).isEmpty)
  }

  test("testFilterToFindFile_ArchiveFileArchivesOnly_Some") {
    val settings = getFindSettings.copy(archivesOnly = true)
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterToFindFile(file).nonEmpty)
  }

  test("testFilterToFindFile_NoExtensionsNoPatterns_Some") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterToFindFile(file).nonEmpty)
  }

  test("testFilterToFindFile_IsFindFile_Some") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterToFindFile(file).nonEmpty)
  }

  test("testFilterToFindFile_NotIsFindFile_None") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterToFindFile(file).isEmpty)
  }

  test("testFilterToFindFile_NonArchiveFileArchivesOnly_None") {
    val settings = getFindSettings.copy(archivesOnly = true)
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterToFindFile(file).isEmpty)
  }
}

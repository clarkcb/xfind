package scalafind

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File

class FinderFilteringTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  def getFindSettings: FindSettings = {
    FindSettings(paths = Set("."))
  }

  /** ***********************************************************
    * isFindDir tests
    * ************************************************************/
  test("testisFindDir_SingleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File(".")))
  }

  test("test testisFindDir_SingleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File(".")))
  }

  test("testisFindDir_DoubleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File("..")))
  }

  test("testisFindDir_IsHidden_False") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(new File(".git")))
  }

  test("testisFindDir_IsHiddenIncludeHidden_True") {
    val settings = getFindSettings.copy(excludeHidden = false)
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File(".git")))
  }

  test("testisFindDir_NoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File("/Users")))
  }

  test("testisFindDir_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(new File("CsFind")))
  }

  test("testisFindDir_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(new File("CsFind")))
  }

  test("testisFindDir_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(new File("CsFind")))
  }

  test("testisFindDir_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    val dir = new File("CsFind")
    assert(finder.isMatchingDir(dir))
  }

  /** ***********************************************************
    * isFindFile tests
    * ************************************************************/
  test("testIsFindFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  test("testIsFindFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  test("testIsFindFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inExtensions = Set("java"))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFile(file))
  }

  test("testIsFindFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFile(file))
  }

  test("testIsFindFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outExtensions = Set("java"))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  test("testIsFindFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FileResult(new File("Finder.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  test("testIsFindFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFile(file))
  }

  test("testIsFindFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val file = new FileResult(new File("Finder.cs"), FileType.Code)
    assert(!finder.isMatchingFile(file))
  }

  test("testIsFindFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
//    val file = new File("FileUtil.cs")
    val file = new FileResult(new File("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFile(file))
  }

  /** ***********************************************************
    * IsArchiveFindFile tests
    * ************************************************************/
  test("testIsArchiveFindFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsArchiveFindFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsArchiveFindFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsArchiveFindFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsArchiveFindFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsArchiveFindFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsArchiveFindFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsArchiveFindFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.isMatchingArchiveFile(file.getName))
  }

  test("testIsArchiveFindFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.isMatchingArchiveFile(file.getName))
  }
}

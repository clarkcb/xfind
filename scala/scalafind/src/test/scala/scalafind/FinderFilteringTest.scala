package scalafind

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File
import java.nio.file.{Files, Path, Paths}

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
    assert(finder.isMatchingDir(Paths.get(".")))
  }

  test("test testisFindDir_SingleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get(".")))
  }

  test("testisFindDir_DoubleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get("..")))
  }

  test("testisFindDir_IsHidden_False") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(Paths.get(".git")))
  }

  test("testisFindDir_IsHiddenIncludeHidden_True") {
    val settings = getFindSettings.copy(excludeHidden = false)
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get(".git")))
  }

  test("testisFindDir_NoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get("/Users")))
  }

  test("testisFindDir_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get("CsFind")))
  }

  test("testisFindDir_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(Paths.get("CsFind")))
  }

  test("testisFindDir_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(Paths.get("CsFind")))
  }

  test("testisFindDir_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    val dir = Paths.get("CsFind")
    assert(finder.isMatchingDir(dir))
  }

  /** ***********************************************************
    * isFindFile tests
    * ************************************************************/
  test("testIsFindFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsFindFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsFindFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inExtensions = Set("java"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsFindFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsFindFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outExtensions = Set("java"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsFindFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("Finder.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsFindFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsFindFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("Finder.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsFindFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
//    val file = new File("FileUtil.cs")
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  /** ***********************************************************
    * IsArchiveFindFile tests
    * ************************************************************/
  test("testIsArchiveFindFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsArchiveFindFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsArchiveFindFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsArchiveFindFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsArchiveFindFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsArchiveFindFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsArchiveFindFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsArchiveFindFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsArchiveFindFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }
}

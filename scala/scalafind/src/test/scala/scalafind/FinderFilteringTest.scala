package scalafind

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.nio.file.{Path, Paths}

class FinderFilteringTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  def getFindSettings: FindSettings = {
    FindSettings(paths = Set(Paths.get(FileUtil.CURRENT_PATH)))
  }

  /** ***********************************************************
    * isMatchingDir tests
    * ************************************************************/
  test("testIsMatchingDir_SingleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get(FileUtil.CURRENT_PATH)))
  }

  test("test testIsMatchingDir_SingleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get(FileUtil.CURRENT_PATH)))
  }

  test("testIsMatchingDir_DoubleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get("..")))
  }

  test("testIsMatchingDir_IsHidden_False") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(Paths.get(".git")))
  }

  test("testIsMatchingDir_IsHiddenIncludeHidden_True") {
    val settings = getFindSettings.copy(includeHidden = true)
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get(".git")))
  }

  test("testIsMatchingDir_NoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get("/Users")))
  }

  test("testIsMatchingDir_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get("CsFind")))
  }

  test("testIsMatchingDir_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outDirPatterns = Set("Find".r))
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(Paths.get("CsFind")))
  }

  test("testIsMatchingDir_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    assert(!finder.isMatchingDir(Paths.get("CsFind")))
  }

  test("testIsMatchingDir_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outDirPatterns = Set("FindFiles".r))
    val finder = new Finder(settings)
    val dir = Paths.get("CsFind")
    assert(finder.isMatchingDir(dir))
  }

  /** ***********************************************************
    * isMatchingFileResult tests
    * ************************************************************/
  test("testIsMatchingFileResult_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inExtensions = Set("java"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outExtensions = Set("java"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("Finder.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("Finder.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
//    val file = new File("FileUtil.cs")
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  /** ***********************************************************
    * isMatchingArchiveFileResult tests
    * ************************************************************/
  test("testIsMatchingArchiveFileResult_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFileResult_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFileResult_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFileResult_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFileResult_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFileResult_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFileResult_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFileResult_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFileResult_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }
}

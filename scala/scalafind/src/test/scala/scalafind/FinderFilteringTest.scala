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
    * isMatchingDir tests
    * ************************************************************/
  test("testIsMatchingDir_SingleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get(".")))
  }

  test("test testIsMatchingDir_SingleDot_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    assert(finder.isMatchingDir(Paths.get(".")))
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
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val fr = new FileResult(path, fileType)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val fr = new FileResult(path, fileType)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inExtensions = Set("java"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val fr = new FileResult(path, fileType)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val fr = new FileResult(path, fileType)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outExtensions = Set("java"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val fr = new FileResult(path, fileType)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val path = Paths.get("Finder.cs")
    val fileType = FileType.Code
    val fr = new FileResult(path, fileType)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val fr = new FileResult(path, fileType)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val path = Paths.get("Finder.cs")
    val fileType = FileType.Code
    val fr = new FileResult(path, fileType)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val fr = new FileResult(path, fileType)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesInMimeType_True") {
    val settings = getFindSettings.copy(inMimeTypes = Set("text/plain"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val mimeType = "text/plain"
    val fr = new FileResult(path, fileType, mimeType = Some(mimeType), None)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchInMimeType_False") {
    val settings = getFindSettings.copy(inMimeTypes = Set("application/json"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val mimeType = "text/plain"
    val fr = new FileResult(path, fileType, mimeType = Some(mimeType), None)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesInMimeTypeWildCard_True") {
    val settings = getFindSettings.copy(inMimeTypes = Set("text/*"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val mimeType = "text/plain"
    val fr = new FileResult(path, fileType, mimeType = Some(mimeType), None)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchInMimeTypeWildCard_False") {
    val settings = getFindSettings.copy(inMimeTypes = Set("application/*"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val mimeType = "text/plain"
    val fr = new FileResult(path, fileType, mimeType = Some(mimeType), None)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesOutMimeType_False") {
    val settings = getFindSettings.copy(outMimeTypes = Set("text/plain"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val mimeType = "text/plain"
    val fr = new FileResult(path, fileType, mimeType = Some(mimeType), None)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchOutMimeType_True") {
    val settings = getFindSettings.copy(outMimeTypes = Set("application/json"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val mimeType = "text/plain"
    val fr = new FileResult(path, fileType, mimeType = Some(mimeType), None)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_MatchesOutMimeTypeWildCard_False") {
    val settings = getFindSettings.copy(outMimeTypes = Set("text/*"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val mimeType = "text/plain"
    val fr = new FileResult(path, fileType, mimeType = Some(mimeType), None)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFileResult_DoesNotMatchOutMimeTypeWildCard_True") {
    val settings = getFindSettings.copy(outMimeTypes = Set("application/*"))
    val finder = new Finder(settings)
    val path = Paths.get("FileUtil.cs")
    val fileType = FileType.Code
    val mimeType = "text/plain"
    val fr = new FileResult(path, fileType, mimeType = Some(mimeType), None)
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

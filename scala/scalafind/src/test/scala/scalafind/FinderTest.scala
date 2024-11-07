package scalafind

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import java.nio.file.{Files, Path, Paths}

import java.io.File
import scala.io.Source

class FinderTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  def getFindSettings: FindSettings = {
    FindSettings(paths = Set(Paths.get(FileUtil.CURRENT_PATH)))
  }

  def getFileLines(file: File): Iterator[String] = {
    val source = Source.fromFile(file)
    val lines = source.getLines()
    source.close()
    lines
  }

  def getBinPath(): String = {
    var xfindPath = Option(System.getenv("XFIND_PATH")) match {
      case Some(path) => path
      case None => System.getenv("HOME") + "/src/xfind"
    }
    xfindPath + "/bin"
  }

  /*************************************************************
   * isMatchingDir tests
   *************************************************************/
  test("testIsMatchingDir_SingleDot_True") {
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

  /*************************************************************
   * isMatchingFile tests
   *************************************************************/
  test("testIsMatchingFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inExtensions = Set("java"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outExtensions = Set("java"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("Finder.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("Finder.cs"), FileType.Code)
    assert(!finder.isMatchingFileResult(fr))
  }

  test("testIsMatchingFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outFilePatterns = Set("Find".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("FileUtil.cs"), FileType.Code)
    assert(finder.isMatchingFileResult(fr))
  }

  /*************************************************************
   * isMatchingArchiveFile tests
   *************************************************************/
  test("testIsMatchingArchiveFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFile_MatchesInExtension_True") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFile_DoesNotMatchInExtension_False") {
    val settings = getFindSettings.copy(inArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFile_MatchesOutExtension_False") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFile_DoesNotMatchOutExtension_True") {
    val settings = getFindSettings.copy(outArchiveExtensions = Set("gz"))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFile_MatchesInPattern_True") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFile_DoesNotMatchInPattern_False") {
    val settings = getFindSettings.copy(inArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFile_MatchesOutPattern_False") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("arch".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(!finder.isMatchingArchiveFileResult(fr))
  }

  test("testIsMatchingArchiveFile_DoesNotMatchOutPattern_True") {
    val settings = getFindSettings.copy(outArchiveFilePatterns = Set("archives".r))
    val finder = new Finder(settings)
    val fr = new FileResult(Paths.get("archive.zip"), FileType.Archive)
    assert(finder.isMatchingArchiveFileResult(fr))
  }

  /*************************************************************
   * filterToFileResult tests
   *************************************************************/
  test("testFilterToFileResult_IsHidden_None") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = Paths.get(".gitignore")
    assert(finder.filterToFileResult(file).isEmpty)
  }

  test("testFilterToFileResult_IsHiddenIncludeHidden_Some") {
    val settings = getFindSettings.copy(includeHidden = true)
    val finder = new Finder(settings)
    val file = Paths.get(".hidden.txt")
//    println("FileUtil.isHidden(\"%s\"): %s".format(file.getFileName.toString,
//      FileUtil.isHidden(file.getFileName.toString)))
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_ArchiveNoFindArchives_None") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = Paths.get("archive.zip")
    assert(finder.filterToFileResult(file).isEmpty)
  }

  test("testFilterToFileResult_ArchiveFindArchives_Some") {
    val settings = getFindSettings.copy(includeArchives = true)
    val finder = new Finder(settings)
    val file = Paths.get("archive.zip")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_IsArchiveFindFile_Some") {
    val settings = getFindSettings.copy(includeArchives = true,
      inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = Paths.get("archive.zip")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_NotIsArchiveFindFile_None") {
    val settings = getFindSettings.copy(outExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = Paths.get("archive.zip")
    assert(finder.filterToFileResult(file).isEmpty)
  }

  test("testFilterToFileResult_ArchiveFileArchivesOnly_Some") {
    val settings = getFindSettings.copy(archivesOnly = true)
    val finder = new Finder(settings)
    val file = Paths.get("archive.zip")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_NoExtensionsNoPatterns_Some") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = Paths.get("FileUtil.cs")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_IsFindFile_Some") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = Paths.get("FileUtil.cs")
    assert(finder.filterToFileResult(file).nonEmpty)
  }

  test("testFilterToFileResult_NotIsFindFile_None") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = Paths.get("FileUtil.cs")
    assert(finder.filterToFileResult(file).isEmpty)
  }

  test("testFilterToFileResult_NonArchiveFileArchivesOnly_None") {
    val settings = getFindSettings.copy(archivesOnly = true)
    val finder = new Finder(settings)
    val file = Paths.get("FileUtil.cs")
    assert(finder.filterToFileResult(file).isEmpty)
  }

  /*************************************************************
   * followSymlinks tests
   *************************************************************/
  test("testFollowSymlinks_Default_Excluded") {
    val settings = FindSettings(paths = Set(Paths.get(getBinPath())))
    val finder = new Finder(settings)
    val fileResults = finder.find()
    assert(fileResults.size < 3)
  }

  test("testFollowSymlinks_FollowSymlinks_Included") {
    val settings = FindSettings(paths = Set(Paths.get(getBinPath())), followSymlinks = true)
    val finder = new Finder(settings)
    val fileResults = finder.find()
    assert(fileResults.isEmpty || fileResults.size > 2)
  }

  test("testFollowSymlinks_NoFollowSymlinks_Excluded") {
    val settings = FindSettings(paths = Set(Paths.get(getBinPath())), followSymlinks = false)
    val finder = new Finder(settings)
    val fileResults = finder.find()
    assert(fileResults.size < 3)
  }
}

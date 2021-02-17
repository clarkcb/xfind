package scalafind

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File
import scala.io.Source

class FinderTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  val testFile1 = new File(getClass.getResource("/testFile1.txt").toURI)
  var lines1: Iterator[String] = Iterator.empty
  var contents1 = ""

  def getFindSettings: FindSettings = {
    val startPath = System.getProperty("user.home") + "/src/xfind/scala/scalafind/src/test/resources"
    FindSettings(startPath = Some(startPath), findPatterns = Set("\\bFinder\\b".r))
  }

  def getFileLines(file: File): Iterator[String] = {
    val source = Source.fromFile(file)
    val lines = source.getLines()
    source.close()
    lines
  }

  override def beforeEach() {
    contents1 = FileUtil.getFileContents(testFile1)
    lines1 = getFileLines(testFile1)
  }

  override def beforeAll() {
    beforeEach()
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
   * IsArchiveFindFile tests
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
   * FilterFile tests
   *************************************************************/
  test("testFilterFile_IsHidden_False") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File(".gitignore")
    assert(!finder.filterFile(file))
  }

  test("testFilterFile_IsHiddenIncludeHidden_True") {
    val settings = getFindSettings.copy(excludeHidden = false)
    val finder = new Finder(settings)
    val file = new File(".hidden.txt")
    println("FileUtil.isHidden(\"%s\"): %s".format(file.getName,
      FileUtil.isHidden(file.getName)))
    assert(finder.filterFile(file))
  }

  test("testFilterFile_ArchiveNoFindArchives_False") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.filterFile(file))
  }

  test("testFilterFile_ArchiveFindArchives_True") {
    val settings = getFindSettings.copy(findArchives = true)
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterFile(file))
  }

  test("testFilterFile_IsArchiveFindFile_True") {
    val settings = getFindSettings.copy(findArchives = true,
      inArchiveExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterFile(file))
  }

  test("testFilterFile_NotIsArchiveFindFile_False") {
    val settings = getFindSettings.copy(outExtensions = Set("zip"))
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(!finder.filterFile(file))
  }

  test("testFilterFile_ArchiveFileArchivesOnly_True") {
    val settings = getFindSettings.copy(archivesOnly = true)
    val finder = new Finder(settings)
    val file = new File("archive.zip")
    assert(finder.filterFile(file))
  }

  test("testFilterFile_NoExtensionsNoPatterns_True") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterFile(file))
  }

  test("testFilterFile_IsFindFile_True") {
    val settings = getFindSettings.copy(inExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(finder.filterFile(file))
  }

  test("testFilterFile_NotIsFindFile_False") {
    val settings = getFindSettings.copy(outExtensions = Set("cs"))
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(!finder.filterFile(file))
  }

  test("testFilterFile_NonArchiveFileArchivesOnly_False") {
    val settings = getFindSettings.copy(archivesOnly = true)
    val finder = new Finder(settings)
    val file = new File("FileUtil.cs")
    assert(!finder.filterFile(file))
  }

  test("test getStartLineIndices") {
    val indices = Finder.getLineIndices(contents1)
    println("contents (%d chars): \"%s\"".format(contents1.length, contents1))
    println("indices: "+indices)
  }

  /************************************************************
   * findLineStringIterator tests
   *************************************************************/
  test("test findLineStringIterator #1 - simple") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val source = Source.fromFile(testFile1)
    val lines = source.getLines()
    val results = finder.findStringIterator(lines)
    source.close()
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.isEmpty && r.linesAfter.isEmpty))
    assert(results.head.line == Some("This is line 3, it includes the word Finder"))
    assert(results.head.lineNum == 3)
    assert(results(1).line == Some("Finder"))
    assert(results(1).lineNum == 7)
  }

  test("test findLineStringIterator #2 - linesBefore+linesAfter") {
    val settings = getFindSettings.copy(linesBefore = 2, linesAfter = 2)
    val finder = new Finder(settings)
    val source = Source.fromFile(testFile1)
    val lines = source.getLines()
    val results = finder.findStringIterator(lines)
    source.close()
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.length == 2 && r.linesAfter.length == 2))
  }

  test("test findLineStringIterator #3 - inLinesBeforeAfterPattern") {
    val settings = getFindSettings.copy(linesBefore = 1, linesAfter = 1,
      inLinesBeforePatterns = Set("line".r), inLinesAfterPatterns = Set("line".r))
    val finder = new Finder(settings)
    val source = Source.fromFile(testFile1)
    val lines = source.getLines()
    val results = finder.findStringIterator(lines)
    source.close()
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 1)
    assert(results.forall(r =>
      r.linesBefore.length == settings.linesBefore &&
        r.linesAfter.length == settings.linesAfter))
  }

  /************************************************************
   * findMultiLineString tests
   *************************************************************/
  test("test findMultiLineString #1 - simple") {
    val settings = getFindSettings
    val finder = new Finder(settings)
    val results = finder.findMultiLineString(contents1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.isEmpty && r.linesAfter.isEmpty))
    assert(results(0).line == Some("This is line 3, it includes the word Finder"))
    assert(results(1).line == Some("Finder"))
  }

  test("test findMultiLineString #2 - linesBefore+linesAfter") {
    val settings = getFindSettings.copy(linesBefore = 2, linesAfter = 2)
    val finder = new Finder(settings)
    val results = finder.findMultiLineString(contents1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.length == 2 && r.linesAfter.length == 2))
  }

  test("test findMultiLineString #3 - inLinesBeforeAfterPattern") {
    val settings = getFindSettings.copy(linesBefore = 1, linesAfter = 1,
      inLinesBeforePatterns = Set("line".r), inLinesAfterPatterns = Set("line".r))
    val finder = new Finder(settings)
    val results = finder.findMultiLineString(contents1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 1)
    assert(results.forall(r =>
      r.linesBefore.length == settings.linesBefore &&
        r.linesAfter.length == settings.linesAfter))
  }
}

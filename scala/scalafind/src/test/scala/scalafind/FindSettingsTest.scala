package scalafind

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class FindSettingsTest extends AnyFunSuite with BeforeAndAfterAll {

  def assertDefaultSettings(settings:FindSettings): Unit = {
    assert(settings.archivesOnly == DefaultFindSettings.archivesOnly)
    assert(settings.debug == DefaultFindSettings.debug)
    assert(settings.followSymlinks == DefaultFindSettings.followSymlinks)
    assert(settings.includeHidden == DefaultFindSettings.includeHidden)
    assert(settings.includeArchives == DefaultFindSettings.includeArchives)
    assert(settings.printDirs == DefaultFindSettings.printDirs)
    assert(settings.printFiles == DefaultFindSettings.printFiles)
    assert(settings.printUsage == DefaultFindSettings.printUsage)
    assert(settings.printVersion == DefaultFindSettings.printVersion)
    assert(settings.sortBy == SortBy.FilePath)
    assert(settings.verbose == DefaultFindSettings.verbose)
  }

  // test defaults
  test("""test default settings""") {
    val settings = FindSettings()
    assertDefaultSettings(settings)
  }

  // test SettingsBuilder
  test("""test SettingsBuilder""") {
    val settings = FindSettings(archivesOnly = true, debug = true,
      inExtensions = Set("java", "scala"))
    assert(settings.archivesOnly)
    //assert(settings.includeArchives)
    assert(settings.debug)
    //assert(settings.verbose)
    assert(settings.inExtensions.size == 2)
    assert(settings.inExtensions.contains("java"))
    assert(settings.inExtensions.contains("scala"))
  }
}

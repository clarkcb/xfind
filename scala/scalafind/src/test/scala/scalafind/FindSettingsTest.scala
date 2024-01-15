package scalafind

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class FindSettingsTest extends AnyFunSuite with BeforeAndAfterAll {

  def assertDefaultSettings(settings:FindSettings): Unit = {
    assert(settings.archivesOnly == DefaultSettings.archivesOnly)
    assert(settings.debug == DefaultSettings.debug)
    assert(settings.includeHidden == DefaultSettings.includeHidden)
    assert(settings.includeArchives == DefaultSettings.includeArchives)
    assert(settings.printDirs == DefaultSettings.printDirs)
    assert(settings.printFiles == DefaultSettings.printFiles)
    assert(settings.printUsage == DefaultSettings.printUsage)
    assert(settings.printVersion == DefaultSettings.printVersion)
    assert(settings.sortBy == SortBy.FilePath)
    assert(settings.verbose == DefaultSettings.verbose)
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

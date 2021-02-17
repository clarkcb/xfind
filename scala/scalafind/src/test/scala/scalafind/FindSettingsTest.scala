package scalafind

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class FindSettingsTest extends AnyFunSuite with BeforeAndAfterAll {

  def assertDefaultSettings(settings:FindSettings) {
    assert(settings.archivesOnly == DefaultSettings.archivesOnly)
    assert(settings.debug == DefaultSettings.debug)
    assert(settings.excludeHidden == DefaultSettings.excludeHidden)
    assert(settings.firstMatch == DefaultSettings.firstMatch)
    assert(settings.linesAfter == DefaultSettings.linesAfter)
    assert(settings.linesBefore == DefaultSettings.linesBefore)
    assert(settings.listDirs == DefaultSettings.listDirs)
    assert(settings.listFiles == DefaultSettings.listFiles)
    assert(settings.listLines == DefaultSettings.listLines)
    assert(settings.multiLineFind == DefaultSettings.multiLineFind)
    assert(settings.printResults == DefaultSettings.printResults)
    assert(settings.printUsage == DefaultSettings.printUsage)
    assert(settings.printVersion == DefaultSettings.printVersion)
    assert(settings.findArchives == DefaultSettings.findArchives)
    assert(settings.uniqueLines == DefaultSettings.uniqueLines)
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
      inExtensions = Set("java", "scala"), findPatterns = Set("Find".r))
    assert(settings.archivesOnly)
    //assert(settings.findArchives)
    assert(settings.debug)
    //assert(settings.verbose)
    assert(settings.inExtensions.size == 2)
    assert(settings.inExtensions.contains("java"))
    assert(settings.inExtensions.contains("scala"))
    assert(settings.findPatterns.size == 1)
    assert(settings.findPatterns.toList.head.toString == "Find")
  }
}

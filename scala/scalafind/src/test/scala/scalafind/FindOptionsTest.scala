package scalafind

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class FindOptionsTest extends AnyFunSuite with BeforeAndAfterAll {

  val startpath = "."
  val requiredArgs = Array(startpath)

  def assertDefaultSettings(settings: FindSettings): Unit = {
    assert(settings.archivesOnly == DefaultSettings.archivesOnly)
    assert(settings.debug == DefaultSettings.debug)
    assert(settings.includeArchives == DefaultSettings.includeArchives)
    assert(settings.includeHidden == DefaultSettings.includeHidden)
    assert(settings.listDirs == DefaultSettings.listDirs)
    assert(settings.listFiles)
    assert(settings.printUsage == DefaultSettings.printUsage)
    assert(settings.printVersion == DefaultSettings.printVersion)
    assert(settings.verbose == DefaultSettings.verbose)
  }

  // test defaults
  test("""test settingsFromArgs with defaults""") {
    val args = requiredArgs
    val settings = FindOptions.settingsFromArgs(args)
    assertDefaultSettings(settings)
    assert(settings.paths.size == 1)
    assert(settings.paths.contains(startpath))
  }

  // test requiredArgs only
  test("""test settingsFromArgs with requiredArgs""") {
    val args = requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.paths.size == 1)
    assert(settings.paths.contains(startpath))
  }

  // test --archivesonly
  test("""test settingsFromArgs with args="--archivesonly" """) {
    val args = Array("--archivesonly") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.archivesOnly)
    assert(settings.includeArchives)
  }

  // test --debug
  test("""test settingsFromArgs with args="--debug" """) {
    val args = Array("--debug") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.debug)
    assert(settings.verbose)
  }

  // test --excludehidden
  test("""test settingsFromArgs with args="--excludehidden" """) {
    val args = Array("--excludehidden") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.includeHidden: " + settings.includeHidden)
    assert(!settings.includeHidden)
  }

  // test -h / --help
  test("""test settingsFromArgs with args="-h" / "--help" """) {
    val shortArgs = Array("-h") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.printUsage)

    val longArgs = Array("--help") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.printUsage)
  }

  // test --includehidden
  test("""test settingsFromArgs with args="--includehidden" """) {
    val args = Array("--includehidden") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.includehidden: " + settings.includeHidden)
    assert(settings.includeHidden)
  }

  // test --in-archivefilepattern
  test("""test settingsFromArgs with args="--in-archivefilepattern find" """) {
    val args = Array("--in-archivefilepattern", "find") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.inArchiveFilePatterns: " + settings.inArchiveFilePatterns)
    assert(settings.inArchiveFilePatterns.size == 1)
    assert(settings.inArchiveFilePatterns.map(_.toString()).contains("find"))
  }

  // test -d / --in-dirpattern
  test("""test settingsFromArgs with args="-d find" """) {
    val shortArgs = Array("-d", "find") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.inDirPatterns: " + shortSettings.inDirPatterns)
    assert(shortSettings.inDirPatterns.size == 1)
    assert(shortSettings.inDirPatterns.map(_.toString()).contains("find"))

    val longArgs = Array("--in-dirpattern", "find") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.inDirPatterns: " + longSettings.inDirPatterns)
    assert(longSettings.inDirPatterns.size == 1)
    assert(longSettings.inDirPatterns.map(_.toString()).contains("find"))
  }

  // test -x / --in-ext
  test("""test settingsFromArgs with args="-x scala" """) {
    val shortArgs = Array("-x", "scala") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.inExtensions: " + shortSettings.inExtensions)
    assert(shortSettings.inExtensions.size == 1)
    assert(shortSettings.inExtensions.toList.head == "scala")
    assert(shortSettings.outExtensions.isEmpty)

    val longArgs = Array("--in-ext", "scala") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.inExtensions: " + longSettings.inExtensions)
    assert(longSettings.inExtensions.size == 1)
    assert(longSettings.inExtensions.toList.head == "scala")
    assert(longSettings.outExtensions.isEmpty)
  }

  // test -x with comma-separated list of exts
  test("""test settingsFromArgs with args="-x java,scala" """) {
    val args = Array("-x", "java,scala") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.inExtensions: " + settings.inExtensions)
    assert(settings.inExtensions.size == 2)
    assert(settings.inExtensions.toList.head == "java")
    assert(settings.inExtensions.toList.last == "scala")
    assert(settings.outExtensions.isEmpty)
  }

  // test -f / --in-filepattern
  test("""test settingsFromArgs with args="-f Find" """) {
    val shortArgs = Array("-f", "Find") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.inFilePatterns: " + shortSettings.inFilePatterns)
    assert(shortSettings.inFilePatterns.size == 1)
    assert(shortSettings.inFilePatterns.map(_.toString()).contains("Find"))

    val longArgs = Array("--in-filepattern", "Find") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.inFilePatterns: " + longSettings.inFilePatterns)
    assert(longSettings.inFilePatterns.size == 1)
    assert(longSettings.inFilePatterns.map(_.toString()).contains("Find"))
  }

  // test --listdirs
  test("""test settingsFromArgs with args="--listdirs" """) {
    val args = Array("--listdirs") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.listDirs)
  }

  // test --listfiles
  test("""test settingsFromArgs with args="--listfiles" """) {
    val args = Array("--listfiles") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.listFiles)
  }

  // test -Z / --excludearchives
  test("""test settingsFromArgs with args="-Z" / "--excludearchives" """) {
    val shortArgs = Array("-Z") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(!shortSettings.includeArchives)

    val longArgs = Array("--excludearchives") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(!longSettings.includeArchives)
  }

  // test --out-archivefilepattern
  test("""test settingsFromArgs with args="--out-archivefilepattern find" """) {
    val args = Array("--out-archivefilepattern", "find") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.outArchiveFilePatterns: " + settings.outArchiveFilePatterns)
    assert(settings.outArchiveFilePatterns.size == 1)
    assert(settings.outArchiveFilePatterns.map(_.toString()).contains("find"))
  }

  // test -D / --out-dirpattern
  test("""test settingsFromArgs with args="-D find" """) {
    val shortArgs = Array("-D", "find") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.outDirPatterns: " + shortSettings.outDirPatterns)
    assert(shortSettings.outDirPatterns.map(_.toString()).contains("find"))

    val longArgs = Array("--out-dirpattern", "find") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.outDirPatterns: " + longSettings.outDirPatterns)
    assert(longSettings.outDirPatterns.map(_.toString()).contains("find"))
  }

  // test -X / --out-ext
  test("""test settingsFromArgs with args="-X scala" """) {
    val shortArgs = Array("-X", "scala") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.outExtensions: " + shortSettings.outExtensions)
    assert(shortSettings.inExtensions.isEmpty)
    assert(shortSettings.outExtensions.size == 1)
    assert(shortSettings.outExtensions.map(_.toString).contains("scala"))

    val longArgs = Array("--out-ext", "scala") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.inExtensions: " + longSettings.inExtensions)
    assert(longSettings.inExtensions.isEmpty)
    assert(longSettings.outExtensions.size == 1)
    assert(longSettings.outExtensions.map(_.toString).contains("scala"))
  }

  // test -X with comma-separated list of exts
  test("""test settingsFromArgs with args="-X java,scala" """) {
    val args = Array("-X", "java,scala") ++ requiredArgs
    println("args: " + args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.outExtensions: " + settings.outExtensions)
    assert(settings.inExtensions.isEmpty)
    assert(settings.outExtensions.size == 2)
    assert(settings.outExtensions.contains("java"))
    assert(settings.outExtensions.contains("scala"))
  }

  // test -F / --out-filepattern
  test("""test settingsFromArgs with args="-F Find" """) {
    val shortArgs = Array("-F", "Find") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.outFilePatterns: " + shortSettings.outFilePatterns)
    assert(shortSettings.outFilePatterns.map(_.toString()).contains("Find"))

    val longArgs = Array("--out-filepattern", "Find") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.outFilePatterns: " + longSettings.outFilePatterns)
    assert(longSettings.outFilePatterns.map(_.toString()).contains("Find"))
  }

  // test -z / --includearchives
  test("""test settingsFromArgs with args="-z" / "--includearchives" """) {
    val shortArgs = Array("-z") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.includeArchives)

    val longArgs = Array("--includearchives") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.includeArchives)
  }

  // test -v / --verbose
  test("""test settingsFromArgs with args="-v" / "--verbose" """) {
    val shortArgs = Array("-v") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.verbose)

    val longArgs = Array("--verbose") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.verbose)
  }

  // test -V / --version
  test("""test settingsFromArgs with args="-V" / "--version" """) {
    val shortArgs = Array("-V") ++ requiredArgs
    println("shortArgs: " + shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.printVersion)

    val longArgs = Array("--version") ++ requiredArgs
    println("longArgs: " + longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.printVersion)
  }

  // testing settings from JSON
  test("""test settingsFromJson""") {
    val ss = FindSettings()
    val json = """{
                 |  "path": "~/src/xfind/",
                 |  "in-ext": ["js","ts"],
                 |  "out-dirpattern": ["build", "node_module", "tests", "typings"],
                 |  "out-filepattern": ["gulpfile", "\\.min\\."],
                 |  "debug": true,
                 |  "includehidden": false
                 |}"""
    val settings = FindOptions.settingsFromJson(json.stripMargin, ss)
    assert(settings.paths.size == 1)
    assert(settings.paths.contains("~/src/xfind/"))
    assert(settings.inExtensions.size == 2)
    assert(settings.inExtensions.contains("js"))
    assert(settings.inExtensions.contains("ts"))
    assert(settings.outDirPatterns.size == 4)
    assert(settings.outFilePatterns.size == 2)
    assert(settings.debug)
    assert(!settings.includeHidden)
  }
}

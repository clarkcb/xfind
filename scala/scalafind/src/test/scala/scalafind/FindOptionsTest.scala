package scalafind

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class FindOptionsTest extends AnyFunSuite with BeforeAndAfterAll {

  val startpath = "."
  val findString = "Find"
  //val requiredArgs = List("-s", findString, startpath)
  val requiredArgs = Array("-s", findString, startpath)

  def assertDefaultSettings(settings: FindSettings) {
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
    //assert(settings.printResults == DefaultSettings.printResults)
    assert(settings.printUsage == DefaultSettings.printUsage)
    assert(settings.printVersion == DefaultSettings.printVersion)
    assert(settings.findArchives == DefaultSettings.findArchives)
    assert(settings.uniqueLines == DefaultSettings.uniqueLines)
    assert(settings.verbose == DefaultSettings.verbose)
  }

  // test defaults
  test("""test settingsFromArgs with defaults""") {
    val args = requiredArgs
    val settings = FindOptions.settingsFromArgs(args)
    assertDefaultSettings(settings)
  }

  // test requiredArgs only
  test("""test settingsFromArgs with requiredArgs""") {
    val args = requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.startpath: "+settings.startPath)
    assert(settings.startPath.contains(startpath))
    assert(settings.findPatterns.size == 1)
    assert(settings.findPatterns.toList.head.toString == findString)
  }

  // test -a / --allmatches
  test("""test settingsFromArgs with args="-a" / "--allmatches" """) {
    val shortArgs = Array("-a") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(!shortSettings.firstMatch)

    val longArgs = Array("--allmatches") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(!longSettings.firstMatch)
  }

  // test --archivesonly
  test("""test settingsFromArgs with args="--archivesonly" """) {
    val args = Array("--archivesonly") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.archivesOnly)
  }

  // test --debug
  test("""test settingsFromArgs with args="--debug" """) {
    val args = Array("--debug") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.debug)
  }

  // test --excludehidden
  test("""test settingsFromArgs with args="--excludehidden" """) {
    val args = Array("--excludehidden") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.excludeHidden: "+settings.excludeHidden)
    assert(settings.excludeHidden)
  }

  // test -1 / --firstmatch
  test("""test settingsFromArgs with args="-1" / "--firstmatch" """) {
    val shortArgs = Array("-1") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.firstMatch)

    val longArgs = Array("--firstmatch") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.firstMatch)
  }

  // test -h / --help
  test("""test settingsFromArgs with args="-h" / "--help" """) {
    val shortArgs = Array("-h") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.printUsage)

    val longArgs = Array("--help") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.printUsage)
  }

  // test --includehidden
  test("""test settingsFromArgs with args="--includehidden" """) {
    val args = Array("--includehidden") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.excludeHidden: "+settings.excludeHidden)
    assert(!settings.excludeHidden)
  }

  // test --in-archivefilepattern
  test("""test settingsFromArgs with args="--in-archivefilepattern find" """) {
    val args = Array("--in-archivefilepattern", "find") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.inArchiveFilePatterns: "+settings.inArchiveFilePatterns)
    assert(settings.inArchiveFilePatterns.size == 1)
    assert(settings.inArchiveFilePatterns.map(_.toString()).contains("find"))
  }

  // test -d / --in-dirpattern
  test("""test settingsFromArgs with args="-d find" """) {
    val shortArgs = Array("-d", "find") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.inDirPatterns: "+shortSettings.inDirPatterns)
    assert(shortSettings.inDirPatterns.size == 1)
    assert(shortSettings.inDirPatterns.map(_.toString()).contains("find"))

    val longArgs = Array("--in-dirpattern", "find") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.inDirPatterns: "+longSettings.inDirPatterns)
    assert(longSettings.inDirPatterns.size == 1)
    assert(longSettings.inDirPatterns.map(_.toString()).contains("find"))
  }

  // test -x / --in-ext
  test("""test settingsFromArgs with args="-x scala" """) {
    val shortArgs = Array("-x", "scala") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.inExtensions: "+shortSettings.inExtensions)
    assert(shortSettings.inExtensions.size == 1)
    assert(shortSettings.inExtensions.toList.head == "scala")
    assert(shortSettings.outExtensions.isEmpty)

    val longArgs = Array("--in-ext", "scala") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.inExtensions: "+longSettings.inExtensions)
    assert(longSettings.inExtensions.size == 1)
    assert(longSettings.inExtensions.toList.head == "scala")
    assert(longSettings.outExtensions.isEmpty)
  }

  // test -x with comma-separated list of exts
  test("""test settingsFromArgs with args="-x java,scala" """) {
    val args = Array("-x", "java,scala") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.inExtensions: "+settings.inExtensions)
    assert(settings.inExtensions.size == 2)
    assert(settings.inExtensions.toList.head == "java")
    assert(settings.inExtensions.toList.last == "scala")
    assert(settings.outExtensions.isEmpty)
  }

  // test -f / --in-filepattern
  test("""test settingsFromArgs with args="-f Find" """) {
    val shortArgs = Array("-f", "Find") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.inFilePatterns: "+shortSettings.inFilePatterns)
    assert(shortSettings.inFilePatterns.size == 1)
    assert(shortSettings.inFilePatterns.map(_.toString()).contains("Find"))

    val longArgs = Array("--in-filepattern", "Find") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.inFilePatterns: "+longSettings.inFilePatterns)
    assert(longSettings.inFilePatterns.size == 1)
    assert(longSettings.inFilePatterns.map(_.toString()).contains("Find"))
  }

  // test --in-linesafterpattern
  test("""test settingsFromArgs with args="--in-linesafterpattern Find" """) {
    val args = Array("--in-linesafterpattern", "Find") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.inLinesAfterPatterns: "+settings.inLinesAfterPatterns)
    assert(settings.inLinesAfterPatterns.size == 1)
    assert(settings.inLinesAfterPatterns.map(_.toString()).contains("Find"))
  }

  // test --in-linesbeforepattern
  test("""test settingsFromArgs with args="--in-linesbeforepattern Find" """) {
    val args = Array("--in-linesbeforepattern", "Find") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.inLinesBeforePatterns: "+settings.inLinesBeforePatterns)
    assert(settings.inLinesBeforePatterns.size == 1)
    assert(settings.inLinesBeforePatterns.map(_.toString()).contains("Find"))
  }

  // test -L / --linesafter
  test("""test settingsFromArgs with args="-L 2" / "--linesafter 2" """) {
    val shortArgs = Array("-L", "2") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.linesAfter == 2)

    val longArgs = Array("--linesafter", "2") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.linesAfter == 2)
  }

  // test --linesaftertopattern
  test("""test settingsFromArgs with args="--linesaftertopattern ^\]$" """) {
    val args = Array("--linesaftertopattern", "^\\]$") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.linesAfterToPatterns: "+settings.linesAfterToPatterns)
    assert(settings.linesAfterToPatterns.size == 1)
    assert(settings.linesAfterToPatterns.toList.head.toString == "^\\]$")
  }

  // test --linesafteruntilpattern
  test("""test settingsFromArgs with args="--linesafteruntilpattern ^\]$" """) {
    val args = Array("--linesafteruntilpattern", "^\\]$") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.linesAfterUntilPatterns: "+settings.linesAfterUntilPatterns)
    assert(settings.linesAfterUntilPatterns.size == 1)
    assert(settings.linesAfterUntilPatterns.toList.head.toString == "^\\]$")
  }

  // test -l / --linesbefore
  test("""test settingsFromArgs with args="-l 2" / "--linesbefore 2" """) {
    val shortArgs = Array("-l", "2") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.linesBefore == 2)

    val longArgs = Array("--linesbefore", "2") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.linesBefore == 2)
  }

  // test --listdirs
  test("""test settingsFromArgs with args="--listdirs" """) {
    val args = Array("--listdirs") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.listDirs)
  }

  // test --listfiles
  test("""test settingsFromArgs with args="--listfiles" """) {
    val args = Array("--listfiles") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.listFiles)
  }

  // test --listlines
  test("""test settingsFromArgs with args="--listlines" """) {
    val args = Array("--listlines") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    assert(settings.listLines)
  }

  // test -m / --multilineoption-REMOVE
  test("""test settingsFromArgs with args="-m" / "--multilineoption-REMOVE" """) {
    val shortArgs = Array("-m") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.multiLineFind)

    val longArgs = Array("--multilineoption-REMOVE") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.multiLineFind)
  }

  // test -P / --noprintmatches
  test("""test settingsFromArgs with args="-P" / "--noprintmatches" """) {
    val shortArgs = Array("-P") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(!shortSettings.printResults)

    val longArgs = Array("--noprintmatches") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(!longSettings.printResults)
  }

  // test -Z / --nofindarchives
  test("""test settingsFromArgs with args="-Z" / "--nofindarchives" """) {
    val shortArgs = Array("-Z") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(!shortSettings.findArchives)

    val longArgs = Array("--nofindarchives") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(!longSettings.findArchives)
  }

  // test --out-archivefilepattern
  test("""test settingsFromArgs with args="--out-archivefilepattern find" """) {
    val args = Array("--out-archivefilepattern", "find") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.outArchiveFilePatterns: "+settings.outArchiveFilePatterns)
    assert(settings.outArchiveFilePatterns.size == 1)
    assert(settings.outArchiveFilePatterns.map(_.toString()).contains("find"))
  }

  // test -D / --out-dirpattern
  test("""test settingsFromArgs with args="-D find" """) {
    val shortArgs = Array("-D", "find") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.outDirPatterns: "+shortSettings.outDirPatterns)
    assert(shortSettings.outDirPatterns.map(_.toString()).contains("find"))

    val longArgs = Array("--out-dirpattern", "find") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.outDirPatterns: "+longSettings.outDirPatterns)
    assert(longSettings.outDirPatterns.map(_.toString()).contains("find"))
  }

  // test -X / --out-ext
  test("""test settingsFromArgs with args="-X scala" """) {
    val shortArgs = Array("-X", "scala") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.outExtensions: "+shortSettings.outExtensions)
    assert(shortSettings.inExtensions.isEmpty)
    assert(shortSettings.outExtensions.size == 1)
    assert(shortSettings.outExtensions.map(_.toString).contains("scala"))

    val longArgs = Array("--out-ext", "scala") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.inExtensions: "+longSettings.inExtensions)
    assert(longSettings.inExtensions.isEmpty)
    assert(longSettings.outExtensions.size == 1)
    assert(longSettings.outExtensions.map(_.toString).contains("scala"))
  }

  // test -X with comma-separated list of exts
  test("""test settingsFromArgs with args="-X java,scala" """) {
    val args = Array("-X", "java,scala") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.outExtensions: "+settings.outExtensions)
    assert(settings.inExtensions.isEmpty)
    assert(settings.outExtensions.size == 2)
    assert(settings.outExtensions.contains("java"))
    assert(settings.outExtensions.contains("scala"))
  }

  // test -F / --out-filepattern
  test("""test settingsFromArgs with args="-F Find" """) {
    val shortArgs = Array("-F", "Find") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    println("shortSettings.outFilePatterns: "+shortSettings.outFilePatterns)
    assert(shortSettings.outFilePatterns.map(_.toString()).contains("Find"))

    val longArgs = Array("--out-filepattern", "Find") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    println("longSettings.outFilePatterns: "+longSettings.outFilePatterns)
    assert(longSettings.outFilePatterns.map(_.toString()).contains("Find"))
  }

  // test --out-linesafterpattern
  test("""test settingsFromArgs with args="--out-linesafterpattern Find" """) {
    val args = Array("--out-linesafterpattern", "Find") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.outLinesAfterPatterns: "+settings.outLinesAfterPatterns)
    assert(settings.outLinesAfterPatterns.size == 1)
    assert(settings.outLinesAfterPatterns.toList.head.toString == "Find")
  }

  // test --out-linesbeforepattern
  test("""test settingsFromArgs with args="--out-linesbeforepattern Find" """) {
    val args = Array("--out-linesbeforepattern", "Find") ++ requiredArgs
    println("args: "+args.toList)
    val settings = FindOptions.settingsFromArgs(args)
    println("settings.outLinesBeforePatterns: "+settings.outLinesBeforePatterns)
    assert(settings.outLinesBeforePatterns.size == 1)
    assert(settings.outLinesBeforePatterns.toList.head.toString == "Find")
  }

  // test -p / --printmatches
  test("""test settingsFromArgs with args="-p" / "--printmatches" """) {
    val shortArgs = Array("-p") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.printResults)

    val longArgs = Array("--printmatches") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.printResults)
  }

  // test -z / --findarchives
  test("""test settingsFromArgs with args="-z" / "--findarchives" """) {
    val shortArgs = Array("-z") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.findArchives)

    val longArgs = Array("--findarchives") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.findArchives)
  }

  // test -u / --uniquelines
  test("""test settingsFromArgs with args="-u" / "--uniquelines" """) {
    val shortArgs = Array("-u") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.uniqueLines)

    val longArgs = Array("--uniquelines") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.uniqueLines)
  }

  // test -v / --verbose
  test("""test settingsFromArgs with args="-v" / "--verbose" """) {
    val shortArgs = Array("-v") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.verbose)

    val longArgs = Array("--verbose") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.verbose)
  }

  // test -V / --version
  test("""test settingsFromArgs with args="-V" / "--version" """) {
    val shortArgs = Array("-V") ++ requiredArgs
    println("shortArgs: "+shortArgs.toList)
    val shortSettings = FindOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.printVersion)

    val longArgs = Array("--version") ++ requiredArgs
    println("longArgs: "+longArgs.toList)
    val longSettings = FindOptions.settingsFromArgs(longArgs)
    assert(longSettings.printVersion)
  }

  // testing settings from JSON
  test("""test settingsFromJson""") {
    val ss = FindSettings()
    val json = """{
                 |  "startpath": "/Users/cary/src/xfind/",
                 |  "in-ext": ["js","ts"],
                 |  "out-dirpattern": ["build", "node_module", "tests", "typings"],
                 |  "out-filepattern": ["gulpfile", "\\.min\\."],
                 |  "findpattern": "Finder",
                 |  "linesbefore": 2,
                 |  "linesafter": 2,
                 |  "debug": true,
                 |  "allmatches": false,
                 |  "includehidden": false
                 |}"""
    val settings = FindOptions.settingsFromJson(json.stripMargin, ss)
    assert(settings.startPath.contains("/Users/cary/src/xfind/"))
    assert(settings.inExtensions.size == 2)
    assert(settings.inExtensions.contains("js"))
    assert(settings.inExtensions.contains("ts"))
    assert(settings.outDirPatterns.size == 4)
    assert(settings.outFilePatterns.size == 2)
    assert(settings.findPatterns.size == 1)
    assert(settings.linesBefore == 2)
    assert(settings.linesAfter == 2)
    assert(settings.debug)
    assert(settings.firstMatch)
    assert(settings.excludeHidden)
  }
}
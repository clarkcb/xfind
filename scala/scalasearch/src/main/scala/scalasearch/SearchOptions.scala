package scalasearch

import scala.collection.mutable
import scala.xml.XML

case class SearchOption(shortarg:String, longarg:String, desc:String) {
  val sortarg =
    if (shortarg.nonEmpty) {
      shortarg.toLowerCase + "@" + longarg.toLowerCase
    } else {
      longarg.toLowerCase
    }
}

object SearchOptions {
  // TODO: move to config file
  private val _searchOptionsXmlPath = "/searchoptions.xml"
  private val _searchOptions = mutable.ListBuffer.empty[SearchOption]

  private def searchOptions: List[SearchOption] = {
    if (_searchOptions.isEmpty) {
      val root = XML.load(getClass.getResourceAsStream(_searchOptionsXmlPath))
      val searchOptionNodes = root \\ "searchoption"
      for (searchOptionNode <- searchOptionNodes) {
        val short = (searchOptionNode \ "@short").text
        val long = (searchOptionNode \ "@long").text
        val desc = searchOptionNode.text.trim
        val option = SearchOption(short, long, desc)
        _searchOptions += option
      }
    }
    List.empty[SearchOption] ++ _searchOptions.sortWith(_.sortarg < _.sortarg)
  }

  val argActionMap = Map[String, ((String, SettingsBuilder) => Unit)](
    "in-archiveext" ->
      ((x: String, sb: SettingsBuilder) => sb.addInArchiveExtensions(x)),
    "in-archivefilepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addInArchiveFilePattern(x)),
    "in-dirpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addInDirPattern(x)),
    "in-ext" ->
      ((x: String, sb: SettingsBuilder) => sb.addInExtensions(x)),
    "in-filepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addInFilePattern(x)),
    "in-linesafterpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addInLinesAfterPattern(x)),
    "in-linesbeforepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addInLinesBeforePattern(x)),
    "linesafter" ->
      ((x: String, sb: SettingsBuilder) => sb.linesAfter = x.toInt),
    "linesaftertopattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addLinesAfterToPattern(x)),
    "linesafteruntilpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addLinesAfterUntilPattern(x)),
    "linesbefore" ->
      ((x: String, sb: SettingsBuilder) => sb.linesBefore = x.toInt),
    "maxlinelength" ->
      ((x: String, sb: SettingsBuilder) => sb.maxLineLength = x.toInt),
    "out-archiveext" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutArchiveExtensions(x)),
    "out-archivefilepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutArchiveFilePattern(x)),
    "out-dirpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutDirPattern(x)),
    "out-ext" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutExtensions(x)),
    "out-filepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutFilePattern(x)),
    "out-linesafterpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutLinesAfterPattern(x)),
    "out-linesbeforepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutLinesBeforePattern(x)),
    "search" ->
      ((x: String, sb: SettingsBuilder) => sb.addSearchPattern(x))
  )

  val flagActionMap = Map[String, (SettingsBuilder => Unit)](
    "archivesonly" ->
      ((sb: SettingsBuilder) => sb.setArchivesOnly()),
    "allmatches" ->
      ((sb: SettingsBuilder) => sb.firstMatch = false),
    "debug" ->
      ((sb: SettingsBuilder) => sb.setDebug()),
    "excludehidden" ->
      ((sb: SettingsBuilder) => sb.excludeHidden = true),
    "firstmatch" ->
      ((sb: SettingsBuilder) => sb.firstMatch = true),
    "help" ->
      ((sb: SettingsBuilder) => sb.printUsage = true),
    "includehidden" ->
      ((sb: SettingsBuilder) => sb.excludeHidden = false),
    "listdirs" ->
      ((sb: SettingsBuilder) => sb.listDirs = true),
    "listfiles" ->
      ((sb: SettingsBuilder) => sb.listFiles = true),
    "listlines" ->
      ((sb: SettingsBuilder) => sb.listLines = true),
    "multilinesearch" ->
      ((sb: SettingsBuilder) => sb.multiLineSearch = true),
    "noprintmatches" ->
      ((sb: SettingsBuilder) => sb.printResults = false),
    "norecursive" ->
      ((sb: SettingsBuilder) => sb.recursive = false),
    "nosearcharchives" ->
      ((sb: SettingsBuilder) => sb.searchArchives = false),
    "printmatches" ->
      ((sb: SettingsBuilder) => sb.printResults = true),
    "recursive" ->
      ((sb: SettingsBuilder) => sb.recursive = true),
    "searcharchives" ->
      ((sb: SettingsBuilder) => sb.searchArchives = true),
    "uniquelines" ->
      ((sb: SettingsBuilder) => sb.uniqueLines = true),
    "verbose" ->
      ((sb: SettingsBuilder) => sb.verbose = true),
    "version" ->
      ((sb: SettingsBuilder) => sb.printVersion = true)
  )

  private def mapFromOptions(options: List[SearchOption]): Map[String,SearchOption] = {
    (options.map(o => (o.longarg, o)) ++
      options.filter(o => o.shortarg.length > 0).map(o => (o.shortarg, o))).toMap
  }

  def settingsFromArgs(args: Array[String]): SearchSettings = {
    val sb = new SettingsBuilder
    val argMap = mapFromOptions(searchOptions.filter(o => argActionMap.contains(o.longarg)))
    val flagMap = mapFromOptions(searchOptions.filter(o => flagActionMap.contains(o.longarg)))
    val switchPattern = """^\-+(\w[\w\-]*)$""".r
    def nextArg(arglist:List[String], sb:SettingsBuilder) {
      arglist match {
        case Nil =>
        case switchPattern(arg) :: tail =>
          if (argMap.contains(arg)) {
            if (tail.nonEmpty) {
              argActionMap(argMap(arg).longarg)(tail.head, sb)
              nextArg(tail.tail, sb)
            } else {
              throw new SearchException("Missing value for arg %s".format(arg))
            }
          } else if (flagMap.contains(arg)) {
            flagActionMap(flagMap(arg).longarg)(sb)
            nextArg(tail, sb)
          } else {
            throw new SearchException("Unknown option: %s".format(arg))
          }
        case arg :: tail =>
          sb.startPath = Some(arg)
          nextArg(tail, sb)
      }
    }
    nextArg(args.toList, sb)
    sb.toSettings
  }

  def usage(status: Int): Unit = {
    Common.log(getUsageString)
    sys.exit(status)
  }

  def getUsageString: String = {
    val sb = new StringBuilder
    sb.append("Usage:\n")
    sb.append(" scalasearch [options] -s <searchpattern> <startpath>\n\n")
    sb.append("Options:\n")
    val optStrings =
      (searchOptions.map(o => if (o.shortarg.nonEmpty) "-" + o.shortarg + "," else "")
        zip searchOptions.map("--" + _.longarg)).map(o => o._1 + o._2)
    val optDescs = searchOptions.map(_.desc)
    val longest = optStrings.map(_.length).max
    val format = " %1$-" + longest + "s  %2$s\n"
    for (i <- optStrings.indices) {
      sb.append(format.format(optStrings(i), optDescs(i)))
    }
    sb.toString()
  }
}

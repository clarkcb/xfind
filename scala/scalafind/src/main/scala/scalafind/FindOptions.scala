package scalafind

import org.json.simple.parser.{JSONParser, ParseException}
import org.json.simple.{JSONArray, JSONObject, JSONValue}

import java.io.{File, IOException, InputStreamReader}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import java.time.{LocalDateTime, LocalDate}
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

case class FindOption(shortarg: Option[String], longarg: String, desc: String) {
  val sortarg: String = shortarg match {
    case Some(sa) => sa.toLowerCase + "@" + longarg.toLowerCase
    case None => longarg.toLowerCase
  }
}

object FindOptions {
  // TODO: move to config file
  private val _findOptionsJsonPath = "/findoptions.json"
  private val _findOptions = mutable.ListBuffer.empty[FindOption]

  private def findOptions: List[FindOption] = {
    if (_findOptions.isEmpty) {
      loadFindOptionsFromJson()
    }
    List.empty[FindOption] ++ _findOptions.sortWith(_.sortarg < _.sortarg)
  }

  private def loadFindOptionsFromJson(): Unit = {
    try {
      val findOptionsInputStream = getClass.getResourceAsStream(_findOptionsJsonPath)
      val obj = new JSONParser().parse(new InputStreamReader(findOptionsInputStream))
      val jsonObj = obj.asInstanceOf[JSONObject]
      val soIt = jsonObj.get("findoptions").asInstanceOf[JSONArray].iterator()
      while (soIt.hasNext) {
        val soObj = soIt.next().asInstanceOf[JSONObject]
        val longArg = soObj.get("long").asInstanceOf[String]
        val shortArg =
          if (soObj.containsKey("short")) {
            Some(soObj.get("short").asInstanceOf[String])
          } else {
            None
          }
        val desc = soObj.get("desc").asInstanceOf[String]
        val option = FindOption(shortArg, longArg, desc)
        _findOptions += option
      }
    } catch {
      case e: ParseException =>
        print(e.getMessage)
      case e: IOException =>
        print(e.getMessage)
    }
  }

  private def addExtensions(exts: String, extensions: Set[String]): Set[String] = {
    extensions ++ exts.split(",").filterNot(_.isEmpty)
  }

  private def getLastModFromString(lastModString: String): Option[LocalDateTime] = {
    try {
      Some(LocalDateTime.parse(lastModString))
    } catch {
      _ => try {
        val maxLastModDate = LocalDate.parse(lastModString, DateTimeFormatter.ISO_LOCAL_DATE)
        Some(maxLastModDate.atTime(0, 0, 0))
      } catch {
        _ => None
      }
    }
  }

  type ArgAction = (String, FindSettings) => FindSettings

  private val argActionMap = Map[String, ArgAction](
    "in-archiveext" ->
      ((s, ss) => ss.copy(inArchiveExtensions = addExtensions(s, ss.inArchiveExtensions))),
    "in-archivefilepattern" ->
      ((s, ss) => ss.copy(inArchiveFilePatterns = ss.inArchiveFilePatterns + s.r)),
    "in-dirpattern" ->
      ((s, ss) => ss.copy(inDirPatterns = ss.inDirPatterns + s.r)),
    "in-ext" ->
      ((s, ss) => ss.copy(inExtensions = addExtensions(s, ss.inExtensions))),
    "in-filepattern" ->
      ((s, ss) => ss.copy(inFilePatterns = ss.inFilePatterns + s.r)),
    "in-filetype" ->
      ((s, ss) => ss.copy(inFileTypes = ss.inFileTypes + FileTypes.fromName(s))),
    "maxlastmod" ->
      ((s, ss) => ss.copy(maxLastMod = getLastModFromString(s))),
    "maxsize" ->
      ((s, ss) => ss.copy(maxSize = s.toInt)),
    "minlastmod" ->
      ((s, ss) => ss.copy(minLastMod = getLastModFromString(s))),
    "minsize" ->
      ((s, ss) => ss.copy(minSize = s.toInt)),
    "out-archiveext" ->
      ((s, ss) => ss.copy(outArchiveExtensions = addExtensions(s, ss.outArchiveExtensions))),
    "out-archivefilepattern" ->
      ((s, ss) => ss.copy(outArchiveFilePatterns = ss.outArchiveFilePatterns + s.r)),
    "out-dirpattern" ->
      ((s, ss) => ss.copy(outDirPatterns = ss.outDirPatterns + s.r)),
    "out-ext" ->
      ((s, ss) => ss.copy(outExtensions = addExtensions(s, ss.outExtensions))),
    "out-filepattern" ->
      ((s, ss) => ss.copy(outFilePatterns = ss.outFilePatterns + s.r)),
    "out-filetype" ->
      ((s, ss) => ss.copy(outFileTypes = ss.outFileTypes + FileTypes.fromName(s))),
    "path" ->
      ((s, ss) => ss.copy(paths = ss.paths + s)),
    "settings-file" ->
      ((s, ss) => settingsFromFile(s, ss)),
    "sort-by" ->
      ((s, ss) => ss.copy(sortBy = SortBy.fromName(s))),
  )

  type FlagAction = (Boolean, FindSettings) => FindSettings

  private val boolFlagActionMap = Map[String, FlagAction](
    "archivesonly" -> ((b, ss) =>
      if (b) ss.copy(archivesOnly = b, includeArchives = b) else ss.copy(archivesOnly = b)),
    "debug" -> ((b, ss) => if (b) ss.copy(debug = b, verbose = b) else ss.copy(debug = b)),
    "excludearchives" -> ((b, ss) => ss.copy(includeArchives = !b)),
    "excludehidden" -> ((b, ss) => ss.copy(excludeHidden = b)),
    "help" -> ((b, ss) => ss.copy(printUsage = b)),
    "includearchives" -> ((b, ss) => ss.copy(includeArchives = b)),
    "includehidden" -> ((b, ss) => ss.copy(excludeHidden = !b)),
    "listdirs" -> ((b, ss) => ss.copy(listDirs = b)),
    "listfiles" -> ((b, ss) => ss.copy(listFiles = b)),
    "norecursive" -> ((b, ss) => ss.copy(recursive = !b)),
    "recursive" -> ((b, ss) => ss.copy(recursive = b)),
    "sort-ascending" -> ((b, ss) => ss.copy(sortDescending = !b)),
    "sort-caseinsensitive" -> ((b, ss) => ss.copy(sortCaseInsensitive = b)),
    "sort-casesensitive" -> ((b, ss) => ss.copy(sortCaseInsensitive = !b)),
    "sort-descending" -> ((b, ss) => ss.copy(sortDescending = b)),
    "verbose" -> ((b, ss) => ss.copy(verbose = b)),
    "version" -> ((b, ss) => ss.copy(printVersion = b))
  )

  private def settingsFromFile(filePath: String, ss: FindSettings): FindSettings = {
    val file: File = new File(filePath)
    if (!file.exists()) {
      throw new FindException("Settings file not found: %s".format(filePath))
    }
    val json: String = FileUtil.getFileContents(file)
    settingsFromJson(json, ss)
  }

  def settingsFromJson(json: String, ss: FindSettings): FindSettings = {
    val obj: AnyRef = JSONValue.parseWithException(json)
    val jsonObject: JSONObject = obj.asInstanceOf[JSONObject]
    @tailrec
    def recSettingsFromJson(keys: List[String], settings: FindSettings): FindSettings = keys match {
      case Nil => settings
      case k :: ks =>
        val v = jsonObject.get(k)
        recSettingsFromJson(ks, applySetting(k, v, settings))
    }
    recSettingsFromJson(jsonObject.keySet().asScala.map(_.toString).toList, ss)
  }

  @tailrec
  def applySetting(arg: String, obj: Any, ss: FindSettings): FindSettings = obj match {
    case s: String =>
      if (this.argActionMap.contains(arg)) {
        argActionMap(arg)(s, ss)
      } else if (arg == "path") {
        ss.copy(paths = ss.paths + arg)
      } else {
        throw new FindException("Invalid option: " + arg)
      }
    case b: Boolean =>
      if (this.boolFlagActionMap.contains(arg)) {
        boolFlagActionMap(arg)(b, ss)
      } else {
        throw new FindException("Invalid option: " + arg)
      }
    case l: Long =>
      applySetting(arg, l.toString, ss)
    case a: JSONArray =>
      applySettings(arg, a.toArray.toList.map(_.toString), ss)
    case _ =>
      throw new FindException("Unsupported data type")
  }

  @tailrec
  def applySettings(arg: String, lst: List[String], ss: FindSettings): FindSettings = lst match {
    case Nil => ss
    case h :: t => applySettings(arg, t, applySetting(arg, h, ss))
  }

  private def getArgMap: Map[String, String] = {
    val longOpts: Map[String, String] = findOptions.map { o => (o.longarg, o.longarg)}.toMap
    val shortOpts = findOptions.filter(_.shortarg.nonEmpty).map {o => (o.shortarg.get, o.longarg)}.toMap
    longOpts ++ shortOpts
  }

  def settingsFromArgs(args: Array[String]): FindSettings = {
    val argMap = getArgMap
    val switchPattern = """^-+(\w[\w\-]*)$""".r
    @tailrec
    def nextArg(arglist: List[String], ss: FindSettings): FindSettings = {
      arglist match {
        case Nil => ss
        case switchPattern(arg) :: tail =>
          argMap.get(arg) match {
            case Some(longArg) =>
              if (argActionMap.contains(longArg)) {
                if (tail.nonEmpty) {
                  nextArg(tail.tail, argActionMap(longArg)(tail.head, ss))
                } else {
                  throw new FindException("Missing value for arg %s".format(arg))
                }
              } else if (boolFlagActionMap.contains(longArg)) {
                if (Set("help", "version").contains(longArg)) {
                  nextArg(Nil, boolFlagActionMap(longArg)(true, ss))
                } else {
                  nextArg(tail, boolFlagActionMap(longArg)(true, ss))
                }
              } else {
                throw new FindException("Invalid option: %s".format(arg))
              }
            case None =>
              throw new FindException("Invalid option: %s".format(arg))
          }
        case arg :: tail =>
          nextArg(tail, ss.copy(paths = ss.paths + arg))
      }
    }
    // default listFiles to true since running as cli
    nextArg(args.toList, FindSettings(listFiles = true))
  }

  def usage(status: Int): Unit = {
    Common.log(getUsageString)
    sys.exit(status)
  }

  def getUsageString: String = {
    val sb = new StringBuilder
    sb.append("Usage:\n")
    sb.append(" scalafind [options] <path> [<path> ...]\n\n")
    sb.append("Options:\n")
    val optPairs = findOptions.map { so =>
      val opts = so.shortarg match {
        case Some(sa) => s"-$sa,--${so.longarg}"
        case None => s"--${so.longarg}"
      }
      (opts, so.desc)
    }
    val longest = optPairs.map(_._1.length).max
    val format = " %1$-" + longest + "s  %2$s\n"
    optPairs.foreach {op =>
      sb.append(format.format(op._1, op._2))
    }
    sb.toString()
  }
}

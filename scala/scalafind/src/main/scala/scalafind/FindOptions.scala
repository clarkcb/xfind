package scalafind

import org.json.{JSONArray, JSONObject, JSONTokener}

import java.io.{IOException, InputStreamReader}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class FindOption(shortArg: Option[String], longArg: String, desc: String) {
  val sortArg: String = shortArg match {
    case Some(sa) => sa.toLowerCase + "@" + longArg.toLowerCase
    case None => longArg.toLowerCase
  }
}

object FindOptions {
  // TODO: move to config file
  private val _findOptionsJsonPath = "/findoptions.json"
  private val _findOptions = mutable.ListBuffer.empty[FindOption]

  private def findOptions: List[FindOption] = {
    val opts =
      if (_findOptions.isEmpty) {
        loadFindOptionsFromJson()
        _findOptions.sortWith(_.sortArg < _.sortArg)
      } else {
        _findOptions
      }
    List.empty[FindOption] ++ opts
  }

  private var _longArgMap = Map.empty[String, String]

  private def longArgMap: Map[String, String] = {
    if (_longArgMap.isEmpty) {
      val longOpts: Map[String, String] = findOptions.map { o => (o.longArg, o.longArg)}.toMap
      val shortOpts = findOptions.filter(_.shortArg.nonEmpty).map { o => (o.shortArg.get, o.longArg)}.toMap
      _longArgMap = longOpts ++ shortOpts ++ Map("path" -> "path")
    }
    _longArgMap
  }

  private def loadFindOptionsFromJson(): Unit = {
    try {
      val findOptionsInputStream = getClass.getResourceAsStream(_findOptionsJsonPath)
      val jsonObj = new JSONObject(new JSONTokener(new InputStreamReader(findOptionsInputStream)))
      val findOptionsArray = jsonObj.getJSONArray("findoptions").iterator()
      while (findOptionsArray.hasNext) {
        val findOptionObj = findOptionsArray.next().asInstanceOf[JSONObject]
        val longArg = findOptionObj.getString("long")
        val shortArg =
          if (findOptionObj.has("short")) {
            Some(findOptionObj.getString("short"))
          } else {
            None
          }
        val desc = findOptionObj.getString("desc")
        val option = FindOption(shortArg, longArg, desc)
        _findOptions += option
      }
    } catch {
      case e: IOException =>
        print(e.getMessage)
    }
  }

  private type BoolAction = (Boolean, FindSettings) => FindSettings

  private val boolActionMap = Map[String, BoolAction](
    "archivesonly" -> ((b, ss) =>
      if (b) ss.copy(archivesOnly = b, includeArchives = b) else ss.copy(archivesOnly = b)),
    "debug" -> ((b, ss) => if (b) ss.copy(debug = b, verbose = b) else ss.copy(debug = b)),
    "excludearchives" -> ((b, ss) => ss.copy(includeArchives = !b)),
    "excludehidden" -> ((b, ss) => ss.copy(includeHidden = !b)),
    "followsymlinks" -> ((b, ss) => ss.copy(followSymlinks = b)),
    "help" -> ((b, ss) => ss.copy(printUsage = b)),
    "includearchives" -> ((b, ss) => ss.copy(includeArchives = b)),
    "includehidden" -> ((b, ss) => ss.copy(includeHidden = b)),
    "nofollowsymlinks" -> ((b, ss) => ss.copy(followSymlinks = !b)),
    "noprintdirs" -> ((b, ss) => ss.copy(printDirs = !b)),
    "noprintfiles" -> ((b, ss) => ss.copy(printFiles = !b)),
    "norecursive" -> ((b, ss) => ss.copy(recursive = !b)),
    "printdirs" -> ((b, ss) => ss.copy(printDirs = b)),
    "printfiles" -> ((b, ss) => ss.copy(printFiles = b)),
    "recursive" -> ((b, ss) => ss.copy(recursive = b)),
    "sort-ascending" -> ((b, ss) => ss.copy(sortDescending = !b)),
    "sort-caseinsensitive" -> ((b, ss) => ss.copy(sortCaseInsensitive = b)),
    "sort-casesensitive" -> ((b, ss) => ss.copy(sortCaseInsensitive = !b)),
    "sort-descending" -> ((b, ss) => ss.copy(sortDescending = b)),
    "verbose" -> ((b, ss) => ss.copy(verbose = b)),
    "version" -> ((b, ss) => ss.copy(printVersion = b))
  )

  private type StringAction = (String, FindSettings) => FindSettings

  private val stringActionMap = Map[String, StringAction](
    "in-archiveext" ->
      ((s, ss) => ss.copy(inArchiveExtensions = ss.addExtensions(s, ss.inArchiveExtensions))),
    "in-archivefilepattern" ->
      ((s, ss) => ss.copy(inArchiveFilePatterns = ss.inArchiveFilePatterns + s.r)),
    "in-dirpattern" ->
      ((s, ss) => ss.copy(inDirPatterns = ss.inDirPatterns + s.r)),
    "in-ext" ->
      ((s, ss) => ss.copy(inExtensions = ss.addExtensions(s, ss.inExtensions))),
    "in-filepattern" ->
      ((s, ss) => ss.copy(inFilePatterns = ss.inFilePatterns + s.r)),
    "in-filetype" ->
      ((s, ss) => ss.copy(inFileTypes = ss.inFileTypes + FileType.forName(s))),
    "maxlastmod" ->
      ((s, ss) => ss.copy(maxLastMod = ss.getLastModFromString(s))),
    "minlastmod" ->
      ((s, ss) => ss.copy(minLastMod = ss.getLastModFromString(s))),
    "out-archiveext" ->
      ((s, ss) => ss.copy(outArchiveExtensions = ss.addExtensions(s, ss.outArchiveExtensions))),
    "out-archivefilepattern" ->
      ((s, ss) => ss.copy(outArchiveFilePatterns = ss.outArchiveFilePatterns + s.r)),
    "out-dirpattern" ->
      ((s, ss) => ss.copy(outDirPatterns = ss.outDirPatterns + s.r)),
    "out-ext" ->
      ((s, ss) => ss.copy(outExtensions = ss.addExtensions(s, ss.outExtensions))),
    "out-filepattern" ->
      ((s, ss) => ss.copy(outFilePatterns = ss.outFilePatterns + s.r)),
    "out-filetype" ->
      ((s, ss) => ss.copy(outFileTypes = ss.outFileTypes + FileType.forName(s))),
    "path" ->
      ((s, ss) => ss.copy(paths = ss.paths + Paths.get(s))),
    "settings-file" ->
      ((s, ss) => updateSettingsFromFile(s, ss)),
    "sort-by" ->
      ((s, ss) => ss.copy(sortBy = SortBy.forName(s))),
  )

  private type IntAction = (Int, FindSettings) => FindSettings

  private val intActionMap = Map[String, IntAction](
    "maxdepth" -> ((i, ss) => ss.copy(maxDepth = i)),
    "mindepth" -> ((i, ss) => ss.copy(minDepth = i)),
  )

  private type LongAction = (Long, FindSettings) => FindSettings

  private val longActionMap = Map[String, LongAction](
    "maxsize" -> ((l, ss) => ss.copy(maxSize = l)),
    "minsize" -> ((l, ss) => ss.copy(minSize = l)),
  )

  @tailrec
  private def applySettings(arg: String, lst: List[Any], ss: FindSettings): FindSettings = lst match {
    case Nil => ss
    case h :: t => applySettings(arg, t, applySetting(arg, h, ss))
  }

  private def applySetting(arg: String, obj: Any, ss: FindSettings): FindSettings = {
    if (this.boolActionMap.contains(arg)) {
      obj match
        case b: Boolean =>
          boolActionMap(arg)(b, ss)
        case _ =>
          throw new FindException("Invalid value for option: " + arg)
    } else if (this.stringActionMap.contains(arg)) {
      obj match
        case s: String =>
          stringActionMap(arg)(s, ss)
        case a: JSONArray =>
          applySettings(arg, a.toList.asScala.toList, ss)
        case _ =>
          throw new FindException("Invalid value for option: " + arg)
    } else if (this.intActionMap.contains(arg)) {
      obj match
        case i: Int =>
          intActionMap(arg)(i, ss)
        case l: Long =>
          intActionMap(arg)(l.toInt, ss)
        case _ =>
          throw new FindException("Invalid value for option: " + arg)
    } else if (this.longActionMap.contains(arg)) {
      obj match
        case i: Int =>
          longActionMap(arg)(i.toLong, ss)
        case l: Long =>
          longActionMap(arg)(l, ss)
        case _ =>
          throw new FindException("Invalid value for option: " + arg)
    } else {
      throw new FindException("Invalid option: " + arg)
    }
  }

  def updateSettingsFromJson(json: String, ss: FindSettings): FindSettings = {
    val jsonObject = new JSONObject(new JSONTokener(json))
    @tailrec
    def recSettingsFromJson(keys: List[String], settings: FindSettings): FindSettings = keys match {
      case Nil => settings
      case k :: ks =>
        val v = jsonObject.get(k)
        recSettingsFromJson(ks, applySetting(k, v, settings))
    }

    // keys are sorted so that output is consistent across all versions
    val keys = jsonObject.keySet().asScala.toList.sorted
    val invalidKeys = keys.filter(k => !longArgMap.contains(k))
    if (invalidKeys.nonEmpty) {
      throw new FindException("Invalid option: %s".format(invalidKeys.head))
    }
    recSettingsFromJson(keys, ss)
  }

  private def updateSettingsFromFile(filePath: String, ss: FindSettings): FindSettings = {
    val path: Path = FileUtil.expandPath(Paths.get(filePath))
    if (!Files.exists(path)) {
      throw new FindException("Settings file not found: %s".format(filePath))
    }
    if (!filePath.endsWith(".json")) {
      throw new FindException("Invalid settings file (must be JSON): %s".format(filePath))
    }
    try {
      val json: String = FileUtil.getPathContents(path)
      updateSettingsFromJson(json, ss)
    } catch {
      case e: IOException =>
        throw new FindException("Error reading settings file: %s".format(filePath))
    }
  }

  def settingsFromArgs(args: Array[String]): FindSettings = {
    val switchPattern = """^-+(\w[\w\-]*)$""".r
    @tailrec
    def nextArg(arglist: List[String], ss: FindSettings): FindSettings = {
      arglist match {
        case Nil => ss
        case switchPattern(arg) :: tail =>
          longArgMap.get(arg) match {
            case Some(longArg) =>
              if (boolActionMap.contains(longArg)) {
                if (Set("help", "version").contains(longArg)) {
                  nextArg(Nil, boolActionMap(longArg)(true, ss))
                } else {
                  nextArg(tail, boolActionMap(longArg)(true, ss))
                }
              } else if (stringActionMap.contains(longArg)
                         || intActionMap.contains(longArg)
                         || longActionMap.contains(longArg)) {
                if (tail.nonEmpty) {
                  if (stringActionMap.contains(longArg)) {
                    nextArg(tail.tail, stringActionMap(longArg)(tail.head, ss))
                  } else if (intActionMap.contains(longArg)) {
                    nextArg(tail.tail, intActionMap(longArg)(tail.head.toInt, ss))
                  } else {
                    nextArg(tail.tail, longActionMap(longArg)(tail.head.toLong, ss))
                  }
                } else {
                  throw new FindException("Missing value for arg %s".format(arg))
                }
              } else {
                throw new FindException("Invalid option: %s".format(arg))
              }
            case None =>
              throw new FindException("Invalid option: %s".format(arg))
          }
        case arg :: tail =>
          nextArg(tail, ss.copy(paths = ss.paths + Paths.get(arg)))
      }
    }
    // default printFiles to true since running as cli
    nextArg(args.toList, FindSettings(printFiles = true))
  }

  def usage(status: Int): Unit = {
    Common.log(getUsageString)
    sys.exit(status)
  }

  private def getUsageString: String = {
    val sb = new StringBuilder
    sb.append("Usage:\n")
    sb.append(" scalafind [options] <path> [<path> ...]\n\n")
    sb.append("Options:\n")
    val optPairs = findOptions.map { so =>
      val opts = so.shortArg match {
        case Some(sa) => s"-$sa,--${so.longArg}"
        case None => s"--${so.longArg}"
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

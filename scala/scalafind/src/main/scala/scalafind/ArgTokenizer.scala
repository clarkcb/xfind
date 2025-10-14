package scalafind

import org.json.{JSONArray, JSONException, JSONObject, JSONTokener}

import java.io.IOException
import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

enum ArgTokenType:
  case Unknown, Bool, Str, Int, Long

case class ArgToken(name: String, tokenType: ArgTokenType, value: Any) {
}

class ArgTokenizer(options: List[FindOption]) {

  private var boolMap: Map[String, String] = Map.empty
  private var strMap: Map[String, String] = Map.empty
  private var intMap: Map[String, String] = Map.empty
  private var longMap: Map[String, String] = Map.empty

  options.foreach { opt =>
    opt.argType match {
      case ArgTokenType.Bool =>
        boolMap += (opt.longArg -> opt.longArg)
        if (opt.shortArg.isDefined) {
          boolMap += (opt.shortArg.get -> opt.longArg)
        }
      case ArgTokenType.Str =>
        strMap += (opt.longArg -> opt.longArg)
        if (opt.shortArg.isDefined) {
          strMap += (opt.shortArg.get -> opt.longArg)
        }
      case ArgTokenType.Int =>
        intMap += (opt.longArg -> opt.longArg)
        if (opt.shortArg.isDefined) {
          intMap += (opt.shortArg.get -> opt.longArg)
        }
      case ArgTokenType.Long =>
        longMap += (opt.longArg -> opt.longArg)
        if (opt.shortArg.isDefined) {
          longMap += (opt.shortArg.get -> opt.longArg)
        }
      case _ => // ignore
    }
  }

  private val longArgWithValRegex: Regex = "^--([a-zA-Z0-9-]+)=(.+)$".r
  private val longArgWithoutValRegex: Regex = "^--([a-zA-Z0-9-]+)$".r
  private val shortArgsRegex: Regex = "^-([a-zA-Z0-9]{2,})$".r
  private val shortArgRegex: Regex = "^-([a-zA-Z0-9])$".r

  private def getLongArg(argName: String): Option[String] = {
    if (boolMap.contains(argName)) {
      boolMap.get(argName)
    } else if (strMap.contains(argName)) {
      strMap.get(argName)
    } else if (intMap.contains(argName)) {
      intMap.get(argName)
    } else if (longMap.contains(argName)) {
      longMap.get(argName)
    } else {
      None
    }
  }

  private def argWantsVal(argName: String): Boolean = {
    strMap.contains(argName) || intMap.contains(argName) || longMap.contains(argName)
  }

  private def updateArgTokens(argName: String, argVal: Option[Any], argTokens: List[ArgToken]): List[ArgToken] = {
    val nextArgTokens =
      if (boolMap.contains(argName)) {
        argVal match {
          case None => List(ArgToken(argName, ArgTokenType.Bool, true))
          case Some(value) if value.isInstanceOf[Boolean] => List(ArgToken(argName, ArgTokenType.Bool, value))
          case Some(value) => throw new FindException("Invalid value for option: %s".format(argName))
        }
      } else if (strMap.contains(argName)) {
        argVal match {
          case None => throw new FindException("Missing value for option: %s".format(argName))
          case Some(value) if value.isInstanceOf[String] => List(ArgToken(argName, ArgTokenType.Str, value))
          case Some(value) if value.isInstanceOf[Iterable[?]] => value.asInstanceOf[Iterable[?]].map(v => ArgToken(argName, ArgTokenType.Str, v))
          case Some(value) if value.isInstanceOf[JSONArray] => value.asInstanceOf[JSONArray].toList.asScala.map(v => ArgToken(argName, ArgTokenType.Str, v))
          case Some(value) => throw new FindException("Invalid value for option: %s".format(argName))
        }
      } else if (intMap.contains(argName)) {
        argVal match {
          case None => throw new FindException("Missing value for option: %s".format(argName))
          case Some(value) if value.isInstanceOf[Int] => List(ArgToken(argName, ArgTokenType.Int, value))
          case Some(value) if value.isInstanceOf[Long] => List(ArgToken(argName, ArgTokenType.Int, value.asInstanceOf[Long].toInt))
          case Some(value) if value.isInstanceOf[String] => List(ArgToken(argName, ArgTokenType.Int, value.asInstanceOf[String].toInt))
          case Some(value) => throw new FindException("Invalid value for option: %s".format(argName))
        }
      } else if (longMap.contains(argName)) {
        argVal match {
          case None => throw new FindException("Missing value for option: %s".format(argName))
          case Some(value) if value.isInstanceOf[Long] => List(ArgToken(argName, ArgTokenType.Long, value))
          case Some(value) if value.isInstanceOf[Int] => List(ArgToken(argName, ArgTokenType.Long, value.asInstanceOf[Int].toLong))
          case Some(value) if value.isInstanceOf[String] => List(ArgToken(argName, ArgTokenType.Long, value.asInstanceOf[String].toLong))
          case Some(value) => throw new FindException("Invalid value for option: %s".format(argName))
        }
      } else {
        throw new FindException("Invalid option: %s".format(argName))
      }
    argTokens :++ nextArgTokens
  }

  @tailrec
  private def recArgTokensFromArgs(args: List[String], argTokens: List[ArgToken]): List[ArgToken] = args match {
    case Nil => argTokens
    case longArgWithValRegex(argName, argVal) :: tail =>
      recArgTokensFromArgs(tail, updateArgTokens(argName, Some(argVal), argTokens))
    case longArgWithoutValRegex(argName) :: tail =>
      val argVal = if (argWantsVal(argName) && tail.nonEmpty) {
        tail.headOption
      } else {
        None
      }
      val nextArgs = if (argVal.isDefined) tail.tail else tail
      recArgTokensFromArgs(nextArgs, updateArgTokens(argName, argVal, argTokens))
    case shortArgsRegex(shortArgs) :: tail =>
      val shortArgsList = shortArgs.toList.map(c => "-%s".format(c))
      recArgTokensFromArgs(shortArgsList ++ tail, argTokens)
    case shortArgRegex(shortArg) :: tail =>
      val longArg = getLongArg(shortArg) match {
        case Some(value) => value
        case None => throw new FindException("Invalid option: %s".format(shortArg))
      }
      val argVal = if (argWantsVal(shortArg) && tail.nonEmpty) {
        tail.headOption
      } else {
        None
      }
      val nextArgs = if (argVal.isDefined) tail.tail else tail
      recArgTokensFromArgs(nextArgs, updateArgTokens(longArg, argVal, argTokens))
    case arg :: tail =>
      recArgTokensFromArgs(tail, updateArgTokens("path", Some(arg), argTokens))
  }

  def tokenizeArgs(args: Array[String]): List[ArgToken] = {
    recArgTokensFromArgs(args.toList, List.empty[ArgToken])
  }

  def tokenizeJson(json: String): List[ArgToken] = {
    @tailrec
    def recArgTokensFromJson(keys: List[String], jsonObject: JSONObject, argTokens: List[ArgToken]): List[ArgToken] = keys match {
      case Nil => argTokens
      case k :: ks =>
        val v = jsonObject.get(k)
        recArgTokensFromJson(ks, jsonObject, updateArgTokens(k, Option(v), argTokens))
    }

    val jsonObject = new JSONObject(new JSONTokener(json))
    // keys are sorted so that output is consistent across all versions
    val keys = jsonObject.keySet().asScala.toList.sorted
    recArgTokensFromJson(keys, jsonObject, List.empty[ArgToken])
  }

  def tokenizeFile(filePath: String): List[ArgToken] = {
    val path: Path = FileUtil.expandPath(Paths.get(filePath))
    if (!Files.exists(path)) {
      throw new FindException("Settings file not found: %s".format(filePath))
    }
    if (!filePath.endsWith(".json")) {
      throw new FindException("Invalid settings file (must be JSON): %s".format(filePath))
    }
    try {
      val json: String = FileUtil.getPathContents(path)
      tokenizeJson(json)
    } catch {
      case e: IOException =>
        throw new FindException("Error reading settings file: %s".format(filePath))
      case e: JSONException =>
        throw new FindException("Error parsing JSON file: %s".format(filePath))
    }
  }
}

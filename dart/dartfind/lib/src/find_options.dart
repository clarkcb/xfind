import 'dart:convert' show json;
import 'dart:io';

import 'package:dartfind/src/arg_tokenizer.dart';
import 'package:dartfind/src/common.dart';
import 'package:dartfind/src/config.dart' show findOptionsPath;
import 'package:dartfind/src/file_types.dart';
import 'package:dartfind/src/find_exception.dart';
import 'package:dartfind/src/find_settings.dart';

class FindOption implements Option {
  final String? _shortArg;
  final String _longArg;
  final String _desc;
  final ArgTokenType _argType;

  const FindOption(this._shortArg, this._longArg, this._desc, this._argType);

  @override
  String? shortArg() => _shortArg;

  @override
  String longArg() => _longArg;

  @override
  String desc() => _desc;

  @override
  ArgTokenType argType() => _argType;

  String sortArg() {
    if (_shortArg == null) {
      return _longArg.toLowerCase();
    } else {
      return '${_shortArg!.toLowerCase()}@${_longArg.toLowerCase()}';
    }
  }

  String optString() {
    if (_shortArg == null) {
      return '--$_longArg';
    } else {
      return '-$_shortArg,--$_longArg';
    }
  }
}

class FindOptions {
  List<FindOption> findOptions = [];
  var boolActionMap = {};
  var stringActionMap = {};
  var intActionMap = {};
  ArgTokenizer? argTokenizer;
  late Future ready;

  FindOptions() {
    setActionMaps();
    ready = loadFindOptionsFromJson().then((f) => setArgTokenizer());
  }

  Future<void> loadFindOptionsFromJson() async {
    var contents = await File(findOptionsPath).readAsString();
    Map soMap = json.decode(contents);
    if (soMap.containsKey('findoptions')) {
      var soList = soMap['findoptions']! as List;
      for (var so in soList) {
        var longArg = (so as Map)['long']!;
        var desc = (so)['desc']!;
        String? shortArg;
        if ((so).containsKey('short')) {
          shortArg = (so)['short']!;
        }
        var argType = ArgTokenType.unknownType;
        if (boolActionMap.containsKey(longArg)) {
          argType = ArgTokenType.boolType;
        } else if (stringActionMap.containsKey(longArg) ||
            longArg == 'settings-file') {
          argType = ArgTokenType.stringType;
        } else if (intActionMap.containsKey(longArg)) {
          argType = ArgTokenType.intType;
        } else {
          throw FindException('No action for option: $longArg');
        }
        findOptions.add(FindOption(shortArg, longArg, desc, argType));
      }
      // Add path manually since it's not in findoptions.json
      findOptions.add(FindOption(null, 'path', '', ArgTokenType.stringType));
    }
  }

  void setActionMaps() {
    boolActionMap = {
      'archivesonly': (bool b, FindSettings ss) => ss.archivesOnly = b,
      'colorize': (bool b, FindSettings ss) => ss.colorize = b,
      'debug': (bool b, FindSettings ss) => ss.debug = b,
      'excludearchives': (bool b, FindSettings ss) => ss.includeArchives = !b,
      'excludehidden': (bool b, FindSettings ss) => ss.includeHidden = !b,
      'followsymlinks': (bool b, FindSettings ss) => ss.followSymlinks = b,
      'help': (bool b, FindSettings ss) => ss.printUsage = b,
      'includearchives': (bool b, FindSettings ss) => ss.includeArchives = b,
      'includehidden': (bool b, FindSettings ss) => ss.includeHidden = b,
      'nocolorize': (bool b, FindSettings ss) => ss.colorize = !b,
      'nofollowsymlinks': (bool b, FindSettings ss) => ss.followSymlinks = !b,
      'noprintdirs': (bool b, FindSettings ss) => ss.printDirs = !b,
      'noprintfiles': (bool b, FindSettings ss) => ss.printFiles = !b,
      'norecursive': (bool b, FindSettings ss) => ss.recursive = !b,
      'printdirs': (bool b, FindSettings ss) => ss.printDirs = b,
      'printfiles': (bool b, FindSettings ss) => ss.printFiles = b,
      'printusage': (bool b, FindSettings ss) => ss.printUsage = b,
      'recursive': (bool b, FindSettings ss) => ss.recursive = b,
      'sort-ascending': (bool b, FindSettings ss) => ss.sortDescending = !b,
      'sort-caseinsensitive': (bool b, FindSettings ss) =>
          ss.sortCaseInsensitive = b,
      'sort-casesensitive': (bool b, FindSettings ss) =>
          ss.sortCaseInsensitive = !b,
      'sort-descending': (bool b, FindSettings ss) => ss.sortDescending = b,
      'verbose': (bool b, FindSettings ss) => ss.verbose = b,
      'version': (bool b, FindSettings ss) => ss.printVersion = b,
    };

    stringActionMap = {
      'in-archiveext': (String s, FindSettings ss) =>
          ss.addExtensions(s, ss.inArchiveExtensions),
      'in-archivefilepattern': (String s, FindSettings ss) =>
          ss.addPattern(s, ss.inArchiveFilePatterns),
      'in-dirpattern': (String s, FindSettings ss) =>
          ss.addPattern(s, ss.inDirPatterns),
      'in-ext': (String s, FindSettings ss) =>
          ss.addExtensions(s, ss.inExtensions),
      'in-filepattern': (String s, FindSettings ss) =>
          ss.addPattern(s, ss.inFilePatterns),
      'in-filetype': (String s, FindSettings ss) =>
          ss.inFileTypes.add(FileTypes.fromName(s)),
      'maxlastmod': (String s, FindSettings ss) =>
          ss.maxLastMod = DateTime.parse(s),
      'minlastmod': (String s, FindSettings ss) =>
          ss.minLastMod = DateTime.parse(s),
      'out-archiveext': (String s, FindSettings ss) =>
          ss.addExtensions(s, ss.outArchiveExtensions),
      'out-archivefilepattern': (String s, FindSettings ss) =>
          ss.addPattern(s, ss.outArchiveFilePatterns),
      'out-dirpattern': (String s, FindSettings ss) =>
          ss.addPattern(s, ss.outDirPatterns),
      'out-ext': (String s, FindSettings ss) =>
          ss.addExtensions(s, ss.outExtensions),
      'out-filepattern': (String s, FindSettings ss) =>
          ss.addPattern(s, ss.outFilePatterns),
      'out-filetype': (String s, FindSettings ss) =>
          ss.outFileTypes.add(FileTypes.fromName(s)),
      'path': (String s, FindSettings ss) => ss.paths.add(s),
      'sort-by': (String s, FindSettings ss) => ss.sortBy = nameToSortBy(s),
    };

    intActionMap = {
      'maxdepth': (int i, FindSettings ss) => ss.maxDepth = i,
      'maxsize': (int i, FindSettings ss) => ss.maxSize = i,
      'mindepth': (int i, FindSettings ss) => ss.minDepth = i,
      'minsize': (int i, FindSettings ss) => ss.minSize = i,
    };
  }

  void setArgTokenizer() {
    argTokenizer = ArgTokenizer(findOptions);
  }

  Future<void> updateSettingsFromArgTokens(
      FindSettings settings, List<ArgToken> argTokens) async {
    await ready.then((_) async {
      for (var argToken in argTokens) {
        if (argToken.tokenType == ArgTokenType.boolType) {
          if (boolActionMap.containsKey(argToken.name)) {
            boolActionMap[argToken.name](argToken.value, settings);
          } else {
            throw FindException('Invalid option: ${argToken.name}');
          }
        } else if (argToken.tokenType == ArgTokenType.stringType) {
          if (stringActionMap.containsKey(argToken.name)) {
            stringActionMap[argToken.name](argToken.value, settings);
          } else if (argToken.name == 'settings-file') {
            await updateSettingsFromFile(settings, argToken.value);
          } else {
            throw FindException('Invalid option: ${argToken.name}');
          }
        } else if (argToken.tokenType == ArgTokenType.intType) {
          if (intActionMap.containsKey(argToken.name)) {
            intActionMap[argToken.name](argToken.value, settings);
          } else {
            throw FindException('Invalid option: ${argToken.name}');
          }
        } else {
          throw FindException('Invalid option: ${argToken.name}');
        }
      }
    });
  }

  Future<void> updateSettingsFromJson(
      FindSettings settings, String jsonString) async {
    await ready.then((_) async {
      List<ArgToken> argTokens = argTokenizer!.tokenizeJson(jsonString);
      await updateSettingsFromArgTokens(settings, argTokens);
    });
  }

  Future<void> updateSettingsFromFile(
      FindSettings settings, String filePath) async {
    List<ArgToken> argTokens = await argTokenizer!.tokenizeFile(filePath);
    await updateSettingsFromArgTokens(settings, argTokens);
  }

  Future<void> updateSettingsFromArgs(
      FindSettings settings, List<String> args) async {
    await ready.then((_) async {
      var argTokens = argTokenizer!.tokenizeArgs(args);
      await updateSettingsFromArgTokens(settings, argTokens);
      return settings;
    });
  }

  Future<FindSettings> settingsFromArgs(List<String> args) async {
    var settings = FindSettings();
    settings.printFiles = true; // default to printing files
    await updateSettingsFromArgs(settings, args);
    return settings;
  }

  Future<String> getUsageString() async {
    return await ready.then((_) {
      var s = 'Usage:\n'
          ' dartfind [options] <path> [<path> ...]\n\n'
          'Options:\n';
      findOptions.removeWhere((o) => o.longArg() == 'path');
      findOptions.sort((o1, o2) => o1.sortArg().compareTo(o2.sortArg()));
      var optStrings = findOptions.map((so) => so.optString()).toList();
      var longest = optStrings.reduce((value, optString) =>
          (optString.length > value.length) ? optString : value);
      for (var i = 0; i < findOptions.length; i++) {
        s += ' ${optStrings[i].padRight(longest.length + 2, ' ')}';
        s += '${findOptions[i].desc()}\n';
      }
      return s;
    });
  }

  void usage() async {
    logMsg(await getUsageString());
  }
}

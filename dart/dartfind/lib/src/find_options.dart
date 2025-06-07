import 'dart:convert' show json;
import 'dart:io';

import 'package:dartfind/src/common.dart';
import 'package:dartfind/src/config.dart' show findOptionsPath;
import 'package:dartfind/src/file_types.dart';
import 'package:dartfind/src/file_util.dart';
import 'package:dartfind/src/find_exception.dart';
import 'package:dartfind/src/find_settings.dart';

class FindOption {
  final String? shortArg;
  final String longArg;
  final String desc;

  const FindOption(this.shortArg, this.longArg, this.desc);

  String sortArg() {
    if (shortArg == null) {
      return longArg.toLowerCase();
    } else {
      return '${shortArg!.toLowerCase()}@${longArg.toLowerCase()}';
    }
  }

  String optString() {
    if (shortArg == null) {
      return '--$longArg';
    } else {
      return '-$shortArg,--$longArg';
    }
  }
}

class FindOptions {
  List<FindOption> findOptions = [];
  var boolActionMap = {};
  var stringActionMap = {};
  var intActionMap = {};
  var longArgMap = {'path': 'path'};
  late Future ready;

  FindOptions() {
    ready = loadFindOptionsFromJson().then((f) => setActionMaps());
  }

  Future<void> loadFindOptionsFromJson() async {
    var contents = await File(findOptionsPath).readAsString();
    Map soMap = json.decode(contents);
    if (soMap.containsKey('findoptions')) {
      var soList = soMap['findoptions']! as List;
      for (var so in soList) {
        var longArg = (so as Map)['long']!;
        longArgMap[longArg] = longArg;
        var desc = (so)['desc']!;
        String? shortArg;
        if ((so).containsKey('short')) {
          shortArg = (so)['short']!;
          longArgMap[shortArg!] = longArg;
        }
        findOptions.add(FindOption(shortArg, longArg, desc));
      }
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

  Future<void> updateSettingsFromJson(
      String jsonString, FindSettings settings) async {
    await ready.then((_) {
      Map jsonMap = json.decode(jsonString);
      var keys = jsonMap.keys.toList();
      // keys are sorted so that output is consistent across all versions
      keys.sort();
      // first check for invalid options
      for (var key in keys) {
        if (!longArgMap.containsKey(key)) {
          throw FindException('Invalid option: $key');
        }
      }
      for (var key in keys) {
        var value = jsonMap[key];
        if (boolActionMap.containsKey(key)) {
          if (value is bool) {
            boolActionMap[key](value, settings);
          } else {
            throw FindException('Invalid value for option: $key');
          }
        } else if (stringActionMap.containsKey(key)) {
          if (value is String) {
            stringActionMap[key](value, settings);
          } else if (value is List) {
            for (var item in value) {
              if (item is String) {
                stringActionMap[key](item, settings);
              } else {
                throw FindException('Invalid value for option: $key');
              }
            }
          } else {
            throw FindException('Invalid value for option: $key');
          }
        } else if (intActionMap.containsKey(key)) {
          if (value is int) {
            intActionMap[key](value, settings);
          } else {
            throw FindException('Invalid value for option: $key');
          }
        } else {
          throw FindException('Invalid option: $key');
        }
      }
    });
  }

  Future<void> updateSettingsFromFile(
      String filePath, FindSettings settings) async {
    var expandedPath = FileUtil.expandPath(filePath);
    if (FileSystemEntity.typeSync(expandedPath) ==
        FileSystemEntityType.notFound) {
      throw FindException('Settings file not found: $filePath');
    }
    if (expandedPath.endsWith('.json')) {
      var contents = await File(expandedPath).readAsString();
      await updateSettingsFromJson(contents, settings);
    } else {
      throw FindException('Invalid settings file (must be JSON): $filePath');
    }
  }

  Future<FindSettings> settingsFromArgs(List<String> args) async {
    return await ready.then((_) async {
      var settings = FindSettings();
      // default printFiles to true since running as cli
      settings.printFiles = true;
      var it = args.iterator;
      while (it.moveNext()) {
        var arg = it.current;
        if (arg.startsWith('-')) {
          while (arg.startsWith('-')) {
            arg = arg.substring(1);
          }
          if (longArgMap.containsKey(arg)) {
            var longArg = longArgMap[arg];
            if (boolActionMap.containsKey(longArg)) {
              boolActionMap[longArg](true, settings);
            } else if (stringActionMap.containsKey(longArg) ||
                intActionMap.containsKey(longArg)) {
              if (it.moveNext()) {
                var s = it.current;
                if (stringActionMap.containsKey(longArg)) {
                  stringActionMap[longArg](s, settings);
                } else {
                  intActionMap[longArg](int.parse(s), settings);
                }
              } else {
                throw FindException('Missing value for option $arg');
              }
            } else if (longArg == 'settings-file') {
              if (it.moveNext()) {
                var s = it.current;
                await updateSettingsFromFile(s, settings);
              } else {
                throw FindException('Missing value for option $arg');
              }
            } else {
              throw FindException('Invalid option: $arg');
            }
          } else {
            throw FindException('Invalid option: $arg');
          }
        } else {
          settings.paths.add(arg);
        }
      }
      return settings;
    });
  }

  Future<String> getUsageString() async {
    return await ready.then((_) {
      var s = 'Usage:\n'
          ' dartfind [options] <path> [<path> ...]\n\n'
          'Options:\n';
      findOptions.sort((o1, o2) => o1.sortArg().compareTo(o2.sortArg()));
      var optStrings = findOptions.map((so) => so.optString()).toList();
      var longest = optStrings.reduce((value, optString) =>
          (optString.length > value.length) ? optString : value);
      for (var i = 0; i < findOptions.length; i++) {
        s += ' ${optStrings[i].padRight(longest.length + 2, ' ')}';
        s += '${findOptions[i].desc}\n';
      }
      return s;
    });
  }

  void usage() async {
    logMsg(await getUsageString());
  }
}

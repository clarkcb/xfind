import 'dart:convert' show json;
import 'dart:io' show File;

import 'package:dartfind/src/common.dart';
import 'package:dartfind/src/config.dart' show FINDOPTIONSPATH;
import 'package:dartfind/src/file_types.dart';
import 'package:dartfind/src/find_exception.dart';
import 'package:dartfind/src/find_settings.dart';

class FindOption {
  final String shortArg;
  final String longArg;
  final String desc;

  const FindOption(this.shortArg, this.longArg, this.desc);

  String sortarg() {
    if (shortArg == null) {
      return longArg.toLowerCase();
    } else {
      return shortArg.toLowerCase() + '@' + longArg.toLowerCase();
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
  var findOptions = [];
  var stringArgMap = {};
  var boolArgMap = {};
  var longArgMap = {};
  Future ready;

  FindOptions() {
    ready = loadFindOptionsFromJson().then((f) => setMaps());
  }

  Future<void> loadFindOptionsFromJson() async {
    var contents = await File(FINDOPTIONSPATH).readAsString();
    Map soMap = json.decode(contents);
    if (soMap.containsKey('findoptions')) {
      var soList = soMap['findoptions'] as List;
      soList.forEach((so) {
        var longArg = (so as Map)['long'];
        longArgMap[longArg] = longArg;
        var desc = (so as Map)['desc'];
        var shortArg;
        if ((so as Map).containsKey('short')) {
          shortArg = (so as Map)['short'];
          longArgMap[shortArg] = longArg;
        }
        findOptions.add(FindOption(shortArg, longArg, desc));
      });
    }
  }

  void setMaps() {
    stringArgMap = {
      'encoding': (String s, FindSettings ss) => ss.textFileEncoding = s,
      'in-archiveext': (String s, FindSettings ss) => ss.addExtensions(s, ss.inArchiveExtensions),
      'in-archivefilepattern': (String s, FindSettings ss) => ss.addPattern(s, ss.inArchiveFilePatterns),
      'in-dirpattern': (String s, FindSettings ss) => ss.addPattern(s, ss.inDirPatterns),
      'in-ext': (String s, FindSettings ss) => ss.addExtensions(s, ss.inExtensions),
      'in-filepattern': (String s, FindSettings ss) => ss.addPattern(s, ss.inFilePatterns),
      'in-filetype': (String s, FindSettings ss) => ss.inFileTypes.add(FileTypes.fromName(s)),
      'in-linesafterpattern': (String s, FindSettings ss) => ss.addPattern(s, ss.inLinesAfterPatterns),
      'in-linesbeforepattern': (String s, FindSettings ss) => ss.addPattern(s, ss.inLinesBeforePatterns),
      'linesafter': (String s, FindSettings ss) => ss.linesAfter = int.parse(s),
      'linesaftertopattern': (String s, FindSettings ss) => ss.addPattern(s, ss.linesAfterToPatterns),
      'linesafteruntilpattern': (String s, FindSettings ss) => ss.addPattern(s, ss.linesAfterUntilPatterns),
      'linesbefore': (String s, FindSettings ss) => ss.linesBefore = int.parse(s),
      'maxlinelength': (String s, FindSettings ss) => ss.maxLineLength = int.parse(s),
      'out-archiveext': (String s, FindSettings ss) => ss.addExtensions(s, ss.outArchiveExtensions),
      'out-archivefilepattern': (String s, FindSettings ss) => ss.addPattern(s, ss.outArchiveFilePatterns),
      'out-dirpattern': (String s, FindSettings ss) => ss.addPattern(s, ss.outDirPatterns),
      'out-ext': (String s, FindSettings ss) => ss.addExtensions(s, ss.outExtensions),
      'out-filepattern': (String s, FindSettings ss) => ss.addPattern(s, ss.outFilePatterns),
      'out-filetype': (String s, FindSettings ss) => ss.outFileTypes.add(FileTypes.fromName(s)),
      'out-linesafterpattern': (String s, FindSettings ss) => ss.addPattern(s, ss.outLinesAfterPatterns),
      'out-linesbeforepattern': (String s, FindSettings ss) => ss.addPattern(s, ss.outLinesBeforePatterns),
      'findpattern': (String s, FindSettings ss) => ss.addPattern(s, ss.findPatterns),
      'startpath': (String s, FindSettings ss) => ss.startPath = s,
    };

    boolArgMap = {
      'archivesonly': (bool b, FindSettings ss) => ss.archivesOnly = b,
      'allmatches': (bool b, FindSettings ss) => ss.firstMatch = !b,
      'colorize': (bool b, FindSettings ss) => ss.colorize = b,
      'debug': (bool b, FindSettings ss) => ss.debug = b,
      'excludehidden': (bool b, FindSettings ss) => ss.excludeHidden = b,
      'firstmatch': (bool b, FindSettings ss) => ss.firstMatch = b,
      'help': (bool b, FindSettings ss) => ss.printUsage = b,
      'includehidden': (bool b, FindSettings ss) => ss.excludeHidden = !b,
      'listdirs': (bool b, FindSettings ss) => ss.listDirs = b,
      'listfiles': (bool b, FindSettings ss) => ss.listFiles = b,
      'listlines': (bool b, FindSettings ss) => ss.listLines = b,
      'multilineoption-REMOVE': (bool b, FindSettings ss) => ss.multiLineFind = b,
      'noprintmatches': (bool b, FindSettings ss) => ss.printResults = !b,
      'printusage': (bool b, FindSettings ss) => ss.printUsage = b,
      'recursive': (bool b, FindSettings ss) => ss.recursive = b,
      'findarchives': (bool b, FindSettings ss) => ss.findArchives = b,
      'uniquelines': (bool b, FindSettings ss) => ss.uniqueLines = b,
      'verbose': (bool b, FindSettings ss) => ss.verbose = b,
      'version': (bool b, FindSettings ss) => ss.printVersion = b,
    };
  }

  Future<void> settingsFromJson(String jsonString, FindSettings settings) async {
    await ready.then((_) {
      Map jsonMap = json.decode(jsonString);
      jsonMap.forEach((key, value) {
        if (stringArgMap.containsKey(key)) {
          if (value is String) {
            stringArgMap[key](value, settings);
          } else if (value is num) {
            stringArgMap[key]('$value', settings);
          } else {
            value.forEach((elem) {
              stringArgMap[key](elem, settings);
            });
          }
        } else if (boolArgMap.containsKey(key)) {
          if (value is bool) {
            boolArgMap[key](value, settings);
          }
        } else {
          logError('Invalid option: $key');
        }
      });
    });
  }

  Future<void> settingsFromFile(String filePath, FindSettings settings) async {
    var contents = await File(filePath).readAsString();
    await settingsFromJson(contents, settings);
  }

  Future<FindSettings> settingsFromArgs(List<String> args) async {
    return await ready.then((_) async {
      var settings = FindSettings();
      // default printResults to true since running as cli
      settings.printResults = true;
      var it = args.iterator;
      while (it.moveNext()) {
        var arg = it.current;
        if (arg.startsWith('-')) {
          while (arg.startsWith('-')) {
            arg = arg.substring(1);
          }
          if (longArgMap.containsKey(arg)) {
            String longArg = longArgMap[arg];
            if (stringArgMap.containsKey(longArg)) {
              if (it.moveNext()) {
                var s = it.current;
                stringArgMap[longArg](s, settings);
              } else {
                throw FindException('Missing value for option $arg');
              }
            } else if (boolArgMap.containsKey(longArg)) {
              boolArgMap[longArg](true, settings);
            } else if (longArg == 'settings-file') {
              if (it.moveNext()) {
                var s = it.current;
                await settingsFromFile(s, settings);
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
          settings.startPath = arg;
        }
      }
      return settings;
    });
  }

  void usage() async {
    log(await getUsageString());
  }

  Future<String> getUsageString() async {
    return await ready.then((_) {
      var s = 'Usage:\n'
          ' dartfind [options] -s <findpattern> <startpath>\n\n'
          'Options:\n';
      var optStrings = findOptions.map((so) => so.optString()).toList();
      var longest = optStrings.reduce((value, optString) => (optString.length > value.length) ? optString : value);
      for (var i=0; i < findOptions.length; i++) {
        s += ' ' + optStrings[i].padRight(longest.length + 2, ' ');
        s += findOptions[i].desc + '\n';
      }
      return s;
    });
  }
}

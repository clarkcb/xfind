import 'dart:convert' show json;
import 'dart:io';

import 'package:dartfind/src/find_exception.dart';

enum ArgTokenType {
  boolType,
  stringType,
  intType,
}

class ArgToken {
  final String name;
  final ArgTokenType tokenType;
  final dynamic value;

  const ArgToken(this.name, this.tokenType, this.value);
}

class ArgTokenizer {
  var boolMap = {};
  var stringMap = {};
  var intMap = {};

  ArgTokenizer(Map<String, String> this.boolMap,
      Map<String, String> this.stringMap, Map<String, String> this.intMap) {}

  List<ArgToken> tokenizeArgs(List<String> args) {
    var argTokens = <ArgToken>[];
    var it = args.iterator;
    while (it.moveNext()) {
      var arg = it.current;
      if (arg.startsWith('-')) {
        var argNames = <String>[];
        String? argVal;
        if (arg.startsWith('--') && arg.length > 2) {
          var argName = arg.substring(2);
          if (argName.contains('=')) {
            var parts = argName.split('=');
            argName = parts[0];
            if (parts.length > 1) {
              argVal = parts.sublist(1).join('=');
            }
          }
          argNames.add(argName);
        } else if (arg.length > 1) {
          for (var i = 1; i < arg.length; i++) {
            if (boolMap.containsKey(arg[i])) {
              argNames.add(boolMap[arg[i]]!);
            } else if (stringMap.containsKey(arg[i])) {
              argNames.add(stringMap[arg[i]]!);
            } else if (intMap.containsKey(arg[i])) {
              argNames.add(intMap[arg[i]]!);
            } else {
              throw FindException('Invalid option: $arg');
            }
          }
        } else {
          throw FindException('Invalid option: $arg');
        }

        for (var argName in argNames) {
          if (boolMap.containsKey(argName)) {
            argTokens
                .add(ArgToken(argName, ArgTokenType.boolType, true));
          } else if (stringMap.containsKey(argName) ||
              intMap.containsKey(argName) ||
              argName == 'settings-file') {
            if (argVal == null) {
              if (it.moveNext()) {
                argVal = it.current;
              } else {
                throw FindException('Missing value for option $arg');
              }
            }
            if (stringMap.containsKey(argName) || argName == 'settings-file') {
              argTokens
                  .add(ArgToken(argName, ArgTokenType.stringType, argVal));
            } else {
              argTokens
                  .add(ArgToken(argName, ArgTokenType.intType, int.parse(argVal)));
            }
          } else {
            throw FindException('Invalid option: $arg');
          }
        }
      } else {
        argTokens.add(ArgToken('path', ArgTokenType.stringType, arg));
      }
    }
    return argTokens;
  }

  List<ArgToken> tokenizeMap(Map<String, dynamic> argMap) {
    var argTokens = <ArgToken>[];
      var keys = argMap.keys.toList();
      // keys are sorted so that output is consistent across all versions
      keys.sort();
      for (var key in keys) {
        var value = argMap[key];
        if (boolMap.containsKey(key)) {
          if (value is bool) {
            argTokens.add(ArgToken(key, ArgTokenType.boolType, value));
          } else {
            throw FindException('Invalid value for option: $key');
          }
        } else if (stringMap.containsKey(key) || key == 'settings-file') {
          if (value is String) {
            argTokens.add(ArgToken(key, ArgTokenType.stringType, value));
          } else if (value is List) {
            for (var item in value) {
              if (item is String) {
                argTokens.add(ArgToken(key, ArgTokenType.stringType, item));
              } else {
                throw FindException('Invalid value for option: $key');
              }
            }
          } else {
            throw FindException('Invalid value for option: $key');
          }
        } else if (intMap.containsKey(key)) {
          if (value is int) {
            argTokens.add(ArgToken(key, ArgTokenType.intType, value));
          } else {
            throw FindException('Invalid value for option: $key');
          }
        // } else if (key == 'settings-file') {
        //   if (value is String) {
        //     await updateSettingsFromFile(settings, value);
        //   } else if (value is List) {
        //     for (var item in value) {
        //       if (item is String) {
        //         await updateSettingsFromFile(settings, item);
        //       } else {
        //         throw FindException('Invalid value for option: $key');
        //       }
        //     }
        //   } else {
        //     throw FindException('Invalid value for option: $key');
        //   }
        } else {
          throw FindException('Invalid option: $key');
        }
      }
    return argTokens;
  }

  List<ArgToken> tokenizeJson(String jsonString) {
    Map jsonMap = json.decode(jsonString);
    return tokenizeMap(jsonMap.cast<String, dynamic>());
  }

  // Note: this method is async because it reads a file, but it returns
  // a List<ArgToken> rather than a Future<List<ArgToken>> because
  // List<ArgToken> tokenizeFile(String filePath) {
  //   var expandedPath = FileUtil.expandPath(filePath);
  //   if (FileSystemEntity.typeSync(expandedPath) ==
  //       FileSystemEntityType.notFound) {
  //     throw FindException('Settings file not found: $filePath');
  //   }
  //   if (expandedPath.endsWith('.json')) {
  //     var contents = await File(expandedPath).readAsString();
  //     return tokenizeJson(contents);
  //   } else {
  //     throw FindException('Invalid settings file (must be JSON): $filePath');
  //   }
  // }
}

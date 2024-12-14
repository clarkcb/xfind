import 'dart:io' show Platform;

import 'package:path/path.dart' as path;
import 'package:dartfind/src/find_exception.dart';

class FileUtil {
  static final dotDirs = {'.', '..'};

  static String extension(String filePath) {
    var ext = path.extension(filePath);
    if (ext.isNotEmpty && ext.startsWith('.')) {
      ext = ext.substring(1);
    }
    return ext;
  }

  static String homePath() {
    if (Platform.environment.containsKey('HOME')) {
      return path.absolute(Platform.environment['HOME']!);
    } else if (Platform.environment.containsKey('USERPROFILE')) {
      return path.absolute(Platform.environment['USERPROFILE']!);
    }
    throw FindException('Home path not found');
  }

  static String expandPath(String filePath) {
    if (filePath.startsWith('~')) {
      var userPath = FileUtil.homePath();
      if (filePath == '~' || filePath == '~${Platform.pathSeparator}') {
        return userPath;
      } else if (filePath.startsWith('~${Platform.pathSeparator}')) {
        return path.join(userPath, filePath.substring(2));
      }

      // Another user's home directory
      var sepIndex = filePath.indexOf(Platform.pathSeparator);
      var homePath = path.dirname(userPath);
      if (sepIndex == -1) {
        var userName = filePath.substring(1);
        return path.join(homePath, userName);
      }
      var userName = filePath.substring(1, sepIndex);
      userPath = path.join(homePath, userName);
      return path.join(userPath, filePath.substring(sepIndex + 1));
    }
    return filePath;
  }

  static String contractPath(String filePath) {
    var homePath = FileUtil.homePath();
    if (filePath.startsWith(homePath)) {
      return filePath.replaceFirst(homePath, '~');
    }
    return filePath;
  }

  static bool isDotDir(String filePath) => dotDirs.contains(filePath);

  static bool isHidden(String filePath) {
    var elems = filePath
        .split(Platform.pathSeparator)
        .where((e) => e.isNotEmpty)
        .toSet();
    return elems.any((elem) => elem.startsWith('.') && !isDotDir(elem));
  }

  static String normalizePath(String filePath) {
    if (filePath.endsWith(Platform.pathSeparator)) {
      return filePath.substring(0, filePath.length - 2);
    }
    return filePath;
  }

  static int sepCount(String filePath) {
    var normPath = normalizePath(filePath);
    return Platform.pathSeparator.allMatches(normPath).length;
  }
}

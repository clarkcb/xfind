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
      var homePath = path.dirname(userPath);
      return path.join(homePath, filePath.substring(1));
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

  static List<String> splitPath(String filePath) {
    return path.split(filePath).where((e) => e.isNotEmpty).toList();
  }

  static Set<String> pathElems(String filePath) {
    return path.split(filePath).where((e) => e.isNotEmpty).toSet();
  }

  static bool isDotDir(String filePath) => dotDirs.contains(filePath);

  static bool isHiddenName(String name) {
    return name.length > 1 && name.startsWith('.') && !isDotDir(name);
  }

  static bool isHiddenPath(String filePath) {
    var elems = pathElems(filePath);
    return elems.any((elem) => isHiddenName(elem));
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

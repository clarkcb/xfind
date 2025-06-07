import 'dart:io' show Directory, File;

import 'package:dartfind/src/console_color.dart';
import 'package:dartfind/src/file_types.dart';
import 'package:dartfind/src/find_settings.dart';
import 'package:path/path.dart';

class FileResult {
  static const String containerSeparator = '!';

  List<File> containers = [];
  final File file;
  final FileType fileType;
  final int fileSize;
  final DateTime? lastMod;

  FileResult(this.file, this.fileType, this.fileSize, this.lastMod);

  @override
  String toString() {
    var s = '';
    if (containers.isNotEmpty) {
      for (var c in containers) {
        s += c.path + containerSeparator;
      }
    }
    s += file.path;
    return s;
  }
}

class FileResultFormatter {
  final FindSettings settings;
  String Function(Directory) formatDirectory = (Directory dir) => dir.path;
  String Function(String) formatFileName = (String fileName) => fileName;

  FileResultFormatter(this.settings) {
    if (settings.colorize) {
      if (settings.inDirPatterns.isNotEmpty) {
        formatDirectory = formatDirectoryWithColor;
      }
      if (settings.inExtensions.isNotEmpty ||
          settings.inFilePatterns.isNotEmpty) {
        formatFileName = formatFileNameWithColor;
      }
    }
  }

  String colorize(String s, int matchStartIndex, int matchEndIndex) {
    var prefix = '';
    if (matchStartIndex > 0) {
      prefix = s.substring(0, matchStartIndex);
    }
    var suffix = '';
    if (matchEndIndex < s.length) {
      suffix = s.substring(matchEndIndex);
    }
    return prefix +
        ConsoleColor.GREEN +
        s.substring(matchStartIndex, matchEndIndex) +
        ConsoleColor.RESET +
        suffix;
  }

  String formatDirectoryWithColor(Directory dir) {
    var formattedDir = dir.path;
    for (var p in settings.inDirPatterns) {
      var match = (p as RegExp).firstMatch(formattedDir);
      if (match != null) {
        formattedDir = colorize(formattedDir, match.start, match.end);
        break;
      }
    }
    return formattedDir;
  }

  String formatFileNameWithColor(String fileName) {
    var formattedFileName = fileName;
    for (var p in settings.inFilePatterns) {
      var match = (p as RegExp).firstMatch(formattedFileName);
      if (match != null) {
        formattedFileName = colorize(formattedFileName, match.start, match.end);
        break;
      }
    }
    if (settings.inExtensions.isNotEmpty) {
      var idx = formattedFileName.lastIndexOf('.');
      if (idx > 0 && idx < formattedFileName.length - 1) {
        formattedFileName =
            colorize(formattedFileName, idx + 1, formattedFileName.length);
      }
    }
    return formattedFileName;
  }

  String formatFile(File file) {
    var parent = formatDirectory(file.parent);
    var fileName = formatFileName(basename(file.path));
    return join(parent, fileName);
  }

  String formatFileResult(FileResult result) {
    return formatFile(result.file);
  }
}

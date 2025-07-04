import 'dart:io' show Directory, File;
import 'package:path/path.dart' as path;

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

class FileResultSorter {
  final FindSettings settings;

  FileResultSorter(this.settings);

  int cmpByFilePath(FileResult fr1, FileResult fr2) {
    String p1 = fr1.file.parent.path;
    String p2 = fr2.file.parent.path;
    if (settings.sortCaseInsensitive) {
      p1 = p1.toLowerCase();
      p2 = p2.toLowerCase();
    }
    if (p1 == p2) {
      String f1 = path.basename(fr1.file.path);
      String f2 = path.basename(fr2.file.path);
      if (settings.sortCaseInsensitive) {
        f1 = f1.toLowerCase();
        f2 = f2.toLowerCase();
      }
      return f1.compareTo(f2);
    }
    return p1.compareTo(p2);
  }

  int cmpByFileName(FileResult fr1, FileResult fr2) {
    String f1 = path.basename(fr1.file.path);
    String f2 = path.basename(fr2.file.path);
    if (settings.sortCaseInsensitive) {
      f1 = f1.toLowerCase();
      f2 = f2.toLowerCase();
    }
    if (f1 == f2) {
      String p1 = fr1.file.parent.path;
      String p2 = fr2.file.parent.path;
      if (settings.sortCaseInsensitive) {
        p1 = p1.toLowerCase();
        p2 = p2.toLowerCase();
      }
      return p1.compareTo(p2);
    }
    return f1.compareTo(f2);
  }

  int cmpByFileSize(FileResult fr1, FileResult fr2) {
    if (fr1.fileSize == fr2.fileSize) {
      return cmpByFilePath(fr1, fr2);
    }
    return fr1.fileSize.compareTo(fr2.fileSize);
  }

  int cmpByFileType(FileResult fr1, FileResult fr2) {
    if (fr1.fileType != fr2.fileType) {
      return fr1.fileType.index.compareTo(fr2.fileType.index);
    }
    return cmpByFilePath(fr1, fr2);
  }

  int cmpByLastMod(FileResult fr1, FileResult fr2) {
    if (fr1.lastMod == null ||
        fr2.lastMod == null ||
        fr1.lastMod!.isAtSameMomentAs(fr2.lastMod!)) {
      return cmpByFilePath(fr1, fr2);
    }
    return fr1.lastMod!.compareTo(fr2.lastMod!);
  }

  int Function(FileResult, FileResult)? getFileResultComparator() {
    if (settings.sortDescending) {
      switch (settings.sortBy) {
        case SortBy.fileName:
          return (FileResult fr1, FileResult fr2) => cmpByFileName(fr2, fr1);
        case SortBy.fileSize:
          return (FileResult fr1, FileResult fr2) => cmpByFileSize(fr2, fr1);
        case SortBy.fileType:
          return (FileResult fr1, FileResult fr2) => cmpByFileType(fr2, fr1);
        case SortBy.lastMod:
          return (FileResult fr1, FileResult fr2) => cmpByLastMod(fr2, fr1);
        default:
          return (FileResult fr1, FileResult fr2) => cmpByFilePath(fr2, fr1);
      }
    }
    switch (settings.sortBy) {
      case SortBy.fileName:
        return (FileResult fr1, FileResult fr2) => cmpByFileName(fr1, fr2);
      case SortBy.fileSize:
        return (FileResult fr1, FileResult fr2) => cmpByFileSize(fr1, fr2);
      case SortBy.fileType:
        return (FileResult fr1, FileResult fr2) => cmpByFileType(fr1, fr2);
      case SortBy.lastMod:
        return (FileResult fr1, FileResult fr2) => cmpByLastMod(fr1, fr2);
      default:
        return (FileResult fr1, FileResult fr2) => cmpByFilePath(fr1, fr2);
    }
  }

  void sort(List<FileResult> fileResults) {
    var fileResultComparator = getFileResultComparator();
    fileResults.sort(fileResultComparator);
  }
}

import 'dart:io';

import 'package:dartfind/dartfind.dart';

void _handleError(err, FindOptions options) {
  logMsg('');
  logError('$err\n');
  options.usage();
  exitCode = 1;
}

List<Directory> getMatchingDirs(List<FileResult> fileResults) {
  Map<String, Directory> pathDirMap = {};
  for (var fr in fileResults) {
    var dir = fr.file.parent;
    pathDirMap[dir.path] = dir;
  }
  var paths = pathDirMap.keys.toList();
  paths.sort();
  List<Directory> dirs = [];
  for (var p in paths) {
    var dir = pathDirMap[p];
    dirs.add(dir!);
  }
  return dirs;
}

void printMatchingDirs(
    List<FileResult> fileResults, FileResultFormatter formatter) {
  var dirs = getMatchingDirs(fileResults);
  if (dirs.isNotEmpty) {
    logMsg('\nMatching directories (${dirs.length}):');
    for (var d in dirs) {
      logMsg(formatter.formatDirectory(d));
    }
  } else {
    logMsg('\nMatching directories: 0');
  }
}

void printMatchingFiles(
    List<FileResult> fileResults, FileResultFormatter formatter) {
  if (fileResults.isNotEmpty) {
    logMsg('\nMatching files (${fileResults.length}):');
    for (var fr in fileResults) {
      logMsg(formatter.formatFileResult(fr));
    }
  } else {
    logMsg('\nMatching files: 0');
  }
}

Future<void> find(FindSettings settings, FindOptions options) async {
  try {
    var finder = Finder(settings);
    await finder.find().then((fileResults) {
      var formatter = FileResultFormatter(settings);
      if (settings.printDirs) {
        printMatchingDirs(fileResults, formatter);
      }

      if (settings.printFiles) {
        printMatchingFiles(fileResults, formatter);
      }
    });
  } on FormatException catch (e) {
    logError(e.message);
  } on FindException catch (e) {
    _handleError(e, options);
  } catch (e) {
    print(e);
    rethrow;
  }
}

Future<void> main(List<String> arguments) async {
  // initialize as success
  exitCode = 0;

  var options = FindOptions();

  await options.settingsFromArgs(arguments).then((settings) {
    if (settings.debug) logMsg('settings: $settings');
    if (settings.printUsage) {
      logMsg('');
      options.usage();
    } else {
      find(settings, options);
    }
  }).catchError((e) {
    _handleError(e, options);
  });
}

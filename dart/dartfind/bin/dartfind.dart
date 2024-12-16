import 'dart:io';

import 'package:dartfind/dartfind.dart';

void _handleError(err, FindOptions options) {
  logMsg('');
  logError('$err\n');
  options.usage();
  exitCode = 1;
}

List<String> getMatchingDirs(List<FileResult> fileResults) {
  var dirs = fileResults.map((fr) => fr.file.parent.path).toSet().toList();
  dirs.sort();
  return dirs;
}

void printMatchingDirs(List<FileResult> fileResults, FindSettings settings) {
  var dirs = getMatchingDirs(fileResults);
  if (dirs.isNotEmpty) {
    logMsg('\nMatching directories (${dirs.length}):');
    if (settings.paths.any((p) => p!.startsWith('~'))) {
      for (var d in dirs) {
        logMsg(FileUtil.contractPath(d));
      }
    } else {
      dirs.forEach(logMsg);
    }
  } else {
    logMsg('\nMatching directories: 0');
  }
}

List<String> getMatchingFiles(List<FileResult> fileResults) {
  return fileResults.map((fr) => fr.toString()).toList();
}

void printMatchingFiles(List<FileResult> fileResults, FindSettings settings) {
  var files = getMatchingFiles(fileResults);
  if (files.isNotEmpty) {
    logMsg('\nMatching files (${files.length}):');
    files.forEach(logMsg);
  } else {
    logMsg('\nMatching files: 0');
  }
}

Future<void> find(FindSettings settings, FindOptions options) async {
  try {
    var finder = Finder(settings);
    await finder.find().then((fileResults) {
      if (settings.printDirs) {
        printMatchingDirs(fileResults, settings);
      }

      if (settings.printFiles) {
        printMatchingFiles(fileResults, settings);
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

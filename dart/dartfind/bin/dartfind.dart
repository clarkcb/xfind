import 'dart:io';

import 'package:dartfind/dartfind.dart';

void _handleError(err, FindOptions options) {
  log('');
  logError('$err\n');
  options.usage();
  exitCode = 1;
}

List<String> getMatchingDirs(List<FileResult> fileResults) {
  var dirs = fileResults.map((f) => f.file.parent.path).toSet().toList();
  dirs.sort();
  return dirs;
}

void printMatchingDirs(List<FileResult> fileResults, FindSettings settings) {
  var dirs = getMatchingDirs(fileResults);
  if (dirs.isNotEmpty) {
    log('\nMatching directories (${dirs.length}):');
    if (settings.paths.any((p) => p.startsWith('~'))) {
      for (var d in dirs) {
        log(FileUtil.contractPath(d));
      }
    } else {
      dirs.forEach(log);
    }
  } else {
    log('\nMatching directories: 0');
  }
}

List<String> getMatchingFiles(List<FileResult> fileResults) {
  return fileResults.map((f) => f.file.path).toList();
}

void printMatchingFiles(List<FileResult> fileResults, FindSettings settings) {
  var files = getMatchingFiles(fileResults);
  if (files.isNotEmpty) {
    log('\nMatching files (${files.length}):');
    if (settings.paths.any((p) => p.startsWith('~'))) {
      for (var f in files) {
        log(FileUtil.contractPath(f));
      }
    } else {
      files.forEach(log);
    }
  } else {
    log('\nMatching files: 0');
  }
}

Future<void> find(FindSettings settings, FindOptions options) async {
  try {
    var finder = Finder(settings);
    await finder.find().then((fileResults) {
      if (fileResults.isNotEmpty) {
        if (settings.listDirs) {
          printMatchingDirs(fileResults, settings);
        }

        if (settings.listFiles) {
          printMatchingFiles(fileResults, settings);
        }
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
    if (settings.debug) log('settings: $settings');
    if (settings.printUsage) {
      log('');
      options.usage();
    } else {
      find(settings, options);
    }
  }).catchError((e) {
    _handleError(e, options);
  });
}

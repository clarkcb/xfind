import 'dart:io';

import 'package:dartfind/dartfind.dart';

void _handleError(err, FindOptions options) {
  log('');
  logError(err.toString() + '\n');
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
      dirs.forEach((d) {
        log(FileUtil.contractPath(d));
      });
    } else {
      dirs.forEach(log);
    }
  } else {
    log('\nMatching directories: 0');
  }
}

int sortFiles(File f1, File f2) {
  if (f1.parent.path == f2.parent.path) {
    return f1.path.compareTo(f2.path);
  } else {
    return f1.parent.path.compareTo(f2.parent.path);
  }
}

List<String> getMatchingFiles(
    List<FileResult> fileResults, FindSettings settings) {
  var files = fileResults.map((f) => f.file).toSet().toList();
  files.sort(sortFiles);
  return files.map((f) => f.path).toList();
}

void printMatchingFiles(List<FileResult> fileResults, FindSettings settings) {
  var files = getMatchingFiles(fileResults, settings);
  if (files.isNotEmpty) {
    log('\nMatching files (${files.length}):');
    if (settings.paths.any((p) => p.startsWith('~'))) {
      files.forEach((f) {
        log(FileUtil.contractPath(f));
      });
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
    await finder.find().then((findFiles) {
      if (findFiles.isNotEmpty) {
        if (settings.listDirs) {
          printMatchingDirs(findFiles, settings);
        }

        if (settings.listFiles) {
          printMatchingFiles(findFiles, settings);
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

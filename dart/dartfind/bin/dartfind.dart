import 'dart:io';

import 'package:dartfind/dartfind.dart';

void _handleError(err, bool colorize, FindOptions options) {
  logMsg('');
  logError('$err\n', colorize);
  options.usage();
  exitCode = 1;
}

Future<void> find(FindSettings settings, FindOptions options) async {
  try {
    var finder = Finder(settings);
    await finder.find().then((fileResults) {
      var formatter = FileResultFormatter(settings);
      if (settings.printDirs) {
        finder.printMatchingDirs(fileResults, formatter);
      }

      if (settings.printFiles) {
        finder.printMatchingFiles(fileResults, formatter);
      }
    });
  } on FormatException catch (e) {
    logError(e.message);
  } on FindException catch (e) {
    _handleError(e, settings.colorize, options);
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
    _handleError(e, true, options);
  });
}

import 'dart:io';

import 'package:dartfind/src/console_color.dart';

void logMsg(String msg) {
  stdout.writeln(msg);
}

void logError(String msg, [bool colorize = true]) {
  var err = colorize
      ? '${ConsoleColor.boldRed}ERROR: $msg${ConsoleColor.reset}'
      : 'ERROR: $msg';
  stderr.writeln(err);
}

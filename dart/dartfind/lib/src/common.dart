import 'dart:io';

void logMsg(String msg) {
  stdout.writeln(msg);
}

void logError(String msg) {
  // TODO: switch to stderr along with all other language versions so output
  //       of all versions will continue to match
  // stderr.writeln('ERROR: $msg');
  stdout.writeln('ERROR: $msg');
}

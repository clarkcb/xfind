import 'dart:io' show File, FileStat;

import 'package:dartfind/src/file_types.dart';

class FileResult {
  static const String containerSeparator = '!';

  List<String> containers = [];
  final File file;
  final FileType fileType;
  final FileStat? stat;

  FileResult(this.file, this.fileType, this.stat);

  @override
  String toString() {
    var s = '';
    if (containers.isNotEmpty) {
      s = containers.join(containerSeparator) + containerSeparator;
    }
    s += file.path;
    return s;
  }
}

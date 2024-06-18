import 'dart:io' show File;

import 'package:dartfind/src/file_types.dart';

class FileResult {
  static const String containerSeparator = '!';

  List<String> containers = [];
  final File file;
  final FileType fileType;
  final int fileSize;
  final DateTime? lastMod;

  FileResult(this.file, this.fileType, this.fileSize, this.lastMod);

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

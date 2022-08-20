import 'dart:io' show File;

import 'package:dartfind/src/file_types.dart';

class FileResult {
  static const String CONTAINER_SEPARATOR = '!';

  List<String> containers = [];
  final File file;
  final FileType fileType;

  FileResult(this.file, this.fileType);

  @override
  String toString() {
    var s = '';
    if (containers.isNotEmpty) {
      s = containers.join(CONTAINER_SEPARATOR) + CONTAINER_SEPARATOR;
    }
    s += file.path;
    return s;
  }
}

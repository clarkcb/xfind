import 'dart:io' show File, Platform;

import 'package:dartfind/dartfind.dart' show FileType, FileResult;
import 'package:test/test.dart';

void main() {
  test('test FileResult with absolute path', () {
    var path =
        '${Platform.environment['HOME']}/src/xfind/dart/dartfind/lib/src/file_result.dart';
    var fr = FileResult(File(path), FileType.code, null);
    expect(fr.toString(), path);
  });

  test('test FileResult with tilde path', () {
    var path = '~/src/xfind/dart/dartfind/lib/src/file_result.dart';
    var fr = FileResult(File(path), FileType.code, null);
    expect(fr.toString(), path);
  });

  test('test FileResult with rel path #1', () {
    var path = './file_result.dart';
    var fr = FileResult(File(path), FileType.code, null);
    expect(fr.toString(), path);
  });

  test('test FileResult with rel path #2', () {
    var path = './file_result.dart';
    var fr = FileResult(File(path), FileType.code, null);
    expect(fr.toString(), path);
  });
}

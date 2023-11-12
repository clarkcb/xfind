import 'package:dartfind/dartfind.dart' show FileTypes, FileType;
import 'package:test/test.dart';

void main() {
  var fileTypes = FileTypes();

  test('test archive file', () async {
    var fileName = 'archive.zip';
    expect(await fileTypes.isArchiveFile(fileName), true);
    expect(await fileTypes.isAudioFile(fileName), false);
    expect(await fileTypes.isBinaryFile(fileName), false);
    expect(await fileTypes.isCodeFile(fileName), false);
    expect(await fileTypes.isFontFile(fileName), false);
    expect(await fileTypes.isImageFile(fileName), false);
    expect(await fileTypes.isTextFile(fileName), false);
    expect(await fileTypes.isVideoFile(fileName), false);
    expect(await fileTypes.isXmlFile(fileName), false);
    expect(await fileTypes.getFileType(fileName), FileType.archive);
  });

  test('test audio file', () async {
    var fileName = 'music.mp3';
    expect(await fileTypes.isArchiveFile(fileName), false);
    expect(await fileTypes.isAudioFile(fileName), true);
    expect(await fileTypes.isBinaryFile(fileName), false);
    expect(await fileTypes.isCodeFile(fileName), false);
    expect(await fileTypes.isFontFile(fileName), false);
    expect(await fileTypes.isImageFile(fileName), false);
    expect(await fileTypes.isTextFile(fileName), false);
    expect(await fileTypes.isVideoFile(fileName), false);
    expect(await fileTypes.isXmlFile(fileName), false);
    expect(await fileTypes.getFileType(fileName), FileType.audio);
  });

  test('test binary file', () async {
    var fileName = 'binary.exe';
    expect(await fileTypes.isArchiveFile(fileName), false);
    expect(await fileTypes.isAudioFile(fileName), false);
    expect(await fileTypes.isBinaryFile(fileName), true);
    expect(await fileTypes.isCodeFile(fileName), false);
    expect(await fileTypes.isFontFile(fileName), false);
    expect(await fileTypes.isImageFile(fileName), false);
    expect(await fileTypes.isTextFile(fileName), false);
    expect(await fileTypes.isVideoFile(fileName), false);
    expect(await fileTypes.isXmlFile(fileName), false);
    expect(await fileTypes.getFileType(fileName), FileType.binary);
  });

  test('test code file', () async {
    var fileName = 'code.dart';
    expect(await fileTypes.isArchiveFile(fileName), false);
    expect(await fileTypes.isAudioFile(fileName), false);
    expect(await fileTypes.isBinaryFile(fileName), false);
    expect(await fileTypes.isCodeFile(fileName), true);
    expect(await fileTypes.isFontFile(fileName), false);
    expect(await fileTypes.isImageFile(fileName), false);
    expect(await fileTypes.isTextFile(fileName), true);
    expect(await fileTypes.isVideoFile(fileName), false);
    expect(await fileTypes.isXmlFile(fileName), false);
    expect(await fileTypes.getFileType(fileName), FileType.code);
  });

  test('test font file', () async {
    var fileName = 'font.ttf';
    expect(await fileTypes.isArchiveFile(fileName), false);
    expect(await fileTypes.isAudioFile(fileName), false);
    expect(await fileTypes.isBinaryFile(fileName), false);
    expect(await fileTypes.isCodeFile(fileName), false);
    expect(await fileTypes.isFontFile(fileName), true);
    expect(await fileTypes.isImageFile(fileName), false);
    expect(await fileTypes.isTextFile(fileName), false);
    expect(await fileTypes.isVideoFile(fileName), false);
    expect(await fileTypes.isXmlFile(fileName), false);
    expect(await fileTypes.getFileType(fileName), FileType.font);
  });

  test('test image file', () async {
    var fileName = 'image.png';
    expect(await fileTypes.isArchiveFile(fileName), false);
    expect(await fileTypes.isAudioFile(fileName), false);
    expect(await fileTypes.isBinaryFile(fileName), false);
    expect(await fileTypes.isCodeFile(fileName), false);
    expect(await fileTypes.isFontFile(fileName), false);
    expect(await fileTypes.isImageFile(fileName), true);
    expect(await fileTypes.isTextFile(fileName), false);
    expect(await fileTypes.isVideoFile(fileName), false);
    expect(await fileTypes.isXmlFile(fileName), false);
    expect(await fileTypes.getFileType(fileName), FileType.image);
  });

  test('test text file', () async {
    var fileName = 'README.md';
    expect(await fileTypes.isArchiveFile(fileName), false);
    expect(await fileTypes.isAudioFile(fileName), false);
    expect(await fileTypes.isBinaryFile(fileName), false);
    expect(await fileTypes.isCodeFile(fileName), false);
    expect(await fileTypes.isFontFile(fileName), false);
    expect(await fileTypes.isImageFile(fileName), false);
    expect(await fileTypes.isTextFile(fileName), true);
    expect(await fileTypes.isVideoFile(fileName), false);
    expect(await fileTypes.isXmlFile(fileName), false);
    expect(await fileTypes.getFileType(fileName), FileType.text);
  });

  test('test video file', () async {
    var fileName = 'movie.mp4';
    expect(await fileTypes.isArchiveFile(fileName), false);
    expect(await fileTypes.isAudioFile(fileName), false);
    expect(await fileTypes.isBinaryFile(fileName), false);
    expect(await fileTypes.isCodeFile(fileName), false);
    expect(await fileTypes.isFontFile(fileName), false);
    expect(await fileTypes.isImageFile(fileName), false);
    expect(await fileTypes.isTextFile(fileName), false);
    expect(await fileTypes.isVideoFile(fileName), true);
    expect(await fileTypes.isXmlFile(fileName), false);
    expect(await fileTypes.getFileType(fileName), FileType.video);
  });

  test('test xml file', () async {
    var fileName = 'markup.xml';
    expect(await fileTypes.isArchiveFile(fileName), false);
    expect(await fileTypes.isAudioFile(fileName), false);
    expect(await fileTypes.isBinaryFile(fileName), false);
    expect(await fileTypes.isCodeFile(fileName), false);
    expect(await fileTypes.isFontFile(fileName), false);
    expect(await fileTypes.isImageFile(fileName), false);
    expect(await fileTypes.isTextFile(fileName), true);
    expect(await fileTypes.isVideoFile(fileName), false);
    expect(await fileTypes.isXmlFile(fileName), true);
    expect(await fileTypes.getFileType(fileName), FileType.xml);
  });

  test('test unknown file', () async {
    var fileName = 'unknown.ZZZ';
    expect(await fileTypes.isArchiveFile(fileName), false);
    expect(await fileTypes.isAudioFile(fileName), false);
    expect(await fileTypes.isBinaryFile(fileName), false);
    expect(await fileTypes.isCodeFile(fileName), false);
    expect(await fileTypes.isFontFile(fileName), false);
    expect(await fileTypes.isImageFile(fileName), false);
    expect(await fileTypes.isTextFile(fileName), false);
    expect(await fileTypes.isVideoFile(fileName), false);
    expect(await fileTypes.isXmlFile(fileName), false);
    expect(await fileTypes.getFileType(fileName), FileType.unknown);
  });
}

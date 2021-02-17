import 'package:dartfind/dartfind.dart' show FindSettings;
import 'package:test/test.dart';

void main() {
  test('test default settings', () {
    var settings = FindSettings();
    expect(settings.archivesOnly, false);
    expect(settings.colorize, true);
    expect(settings.debug, false);
    expect(settings.excludeHidden, true);
    expect(settings.firstMatch, false);
    expect(settings.linesAfter, 0);
    expect(settings.linesBefore, 0);
    expect(settings.listDirs, false);
    expect(settings.listFiles, false);
    expect(settings.listLines, false);
    expect(settings.maxLineLength, 150);
    expect(settings.multiLineFind, false);
    expect(settings.printResults, false);
    expect(settings.printUsage, false);
    expect(settings.printVersion, false);
    expect(settings.findArchives, false);
    expect(settings.recursive, true);
    expect(settings.uniqueLines, false);
    expect(settings.verbose, false);
  });

  test('test add extensions', () {
    var settings = FindSettings();
    settings.addExtensions('dart,kt', settings.inExtensions);
    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('dart'), true);
    expect(settings.inExtensions.contains('kt'), true);
  });

  test('test add extensionsList', () {
    var settings = FindSettings();
    settings.addExtensionsList(['dart', 'kt'], settings.inExtensions);
    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('dart'), true);
    expect(settings.inExtensions.contains('kt'), true);
  });

  test('test add pattern', () {
    var settings = FindSettings();
    settings.addPattern('Finder', settings.findPatterns);
    expect(settings.findPatterns.length, 1);
    expect(settings.findPatterns.first is RegExp, true);
    expect((settings.findPatterns.first as RegExp).pattern, 'Finder');
    expect((settings.findPatterns.first as RegExp).isMultiLine, true);
  });

  test('test set archivesOnly', () {
    var settings = FindSettings();
    expect(settings.archivesOnly, false);
    expect(settings.findArchives, false);
    settings.archivesOnly = true;
    expect(settings.archivesOnly, true);
    expect(settings.findArchives, true);
  });

  test('test set debug', () {
    var settings = FindSettings();
    expect(settings.debug, false);
    expect(settings.verbose, false);
    settings.debug = true;
    expect(settings.debug, true);
    expect(settings.verbose, true);
  });
}

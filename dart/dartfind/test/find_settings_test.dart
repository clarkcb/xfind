import 'package:dartfind/dartfind.dart' show FindSettings;
import 'package:test/test.dart';

void main() {
  test('test default settings', () {
    var settings = FindSettings();
    expect(settings.archivesOnly, false);
    expect(settings.debug, false);
    expect(settings.includeArchives, false);
    expect(settings.includeHidden, false);
    expect(settings.listDirs, false);
    expect(settings.listFiles, false);
    expect(settings.printUsage, false);
    expect(settings.printVersion, false);
    expect(settings.recursive, true);
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
    settings.addPattern('Find', settings.inFilePatterns);
    expect(settings.inFilePatterns.length, 1);
    expect(settings.inFilePatterns.first is RegExp, true);
    expect((settings.inFilePatterns.first as RegExp).pattern, 'Find');
    expect((settings.inFilePatterns.first as RegExp).isMultiLine, true);
  });

  test('test set archivesOnly', () {
    var settings = FindSettings();
    expect(settings.archivesOnly, false);
    expect(settings.includeArchives, false);
    settings.archivesOnly = true;
    expect(settings.archivesOnly, true);
    expect(settings.includeArchives, true);
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

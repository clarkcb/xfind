import 'package:dartfind/dartfind.dart' show FindOptions, FindSettings;
import 'package:test/test.dart';

void main() {
  test('test get settings from minimal args', () async {
    var options = FindOptions();
    var settings = await options.settingsFromArgs(['-s', 'Find', '.']);
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
    expect(settings.printResults, true);
    expect(settings.printUsage, false);
    expect(settings.printVersion, false);
    expect(settings.findArchives, false);
    expect(settings.recursive, true);
    expect(settings.uniqueLines, false);
    expect(settings.verbose, false);
    expect(settings.findPatterns.length, 1);
    expect((settings.findPatterns.first as RegExp).pattern, 'Find');
    expect(settings.startPath, '.');
  });

  test('test get settings from valid args', () async {
    var options = FindOptions();
    var settings = await options.settingsFromArgs(['-x', 'dart,kt', '-s', 'Find', '.']);
    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('dart'), true);
    expect(settings.inExtensions.contains('kt'), true);
    expect(settings.findPatterns.length, 1);
    expect((settings.findPatterns.first as RegExp).pattern, 'Find');
    expect(settings.startPath, '.');
  });

  test('test get settings from json', () async {
    var json = '{'
    '"startpath": "~/src/xfind/",'
    '"in-ext": ["js","ts"],'
    '"out-dirpattern": ["build", "node_module", "tests", "typings"],'
    r'"out-filepattern": ["gulpfile", "\\.min\\."],'
    '"findpattern": "Finder",'
    '"linesbefore": 2,'
    '"linesafter": 2,'
    '"debug": true,'
    '"allmatches": false,'
    '"includehidden": false'
    '}';
    var options = FindOptions();
    var settings = FindSettings();
    await options.settingsFromJson(json, settings);

    expect(settings.startPath, '~/src/xfind/');

    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('js'), true);
    expect(settings.inExtensions.contains('ts'), true);

    expect(settings.outDirPatterns.length, 4);
    expect(settings.outDirPatterns.any((p) => (p as RegExp).pattern == 'node_module'), true);

    expect(settings.outFilePatterns.length, 2);
    expect(settings.outFilePatterns.any((p) => (p as RegExp).pattern == 'gulpfile'), true);

    expect(settings.findPatterns.length, 1);
    expect((settings.findPatterns.first as RegExp).pattern, 'Finder');

    expect(settings.linesBefore, 2);
    expect(settings.linesAfter, 2);

    expect(settings.debug, true);
    expect(settings.verbose, true);
    expect(settings.firstMatch, true);
    expect(settings.excludeHidden, true);
  });
}

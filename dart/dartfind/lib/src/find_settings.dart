import 'package:dartfind/src/file_types.dart';

class FindSettings {
  bool _archivesOnly = false;
  bool get archivesOnly => _archivesOnly;
  set archivesOnly(bool value) {
    _archivesOnly = value;
    if (value) {
      includeArchives = value;
    }
  }

  bool _debug = false;
  bool get debug => _debug;
  set debug(bool value) {
    _debug = value;
    if (value) {
      verbose = value;
    }
  }

  bool excludeHidden = true;

  var inArchiveExtensions = <String>{};
  var inArchiveFilePatterns = <Pattern>{};
  var inDirPatterns = <Pattern>{};
  var inExtensions = <String>{};
  var inFilePatterns = <Pattern>{};

  var inFileTypes = <FileType>{};

  bool includeArchives = false;
  bool listDirs = false;
  bool listFiles = false;

  var outArchiveExtensions = <String>{};
  var outArchiveFilePatterns = <Pattern>{};
  var outDirPatterns = <Pattern>{};
  var outExtensions = <String>{};
  var outFilePatterns = <Pattern>{};

  var outFileTypes = <FileType>{};

  var paths = <String>{};

  bool printUsage = false;
  bool printVersion = false;
  bool recursive = true;
  bool verbose = false;

  void addExtensions(String exts, Set<String> extensions) {
    var extList = exts.split(',').where((ext) => ext.isNotEmpty).toList();
    addExtensionsList(extList, extensions);
  }

  void addExtensionsList(List<String> exts, Set<String> extensions) {
    exts.forEach((ext) {
      extensions.add(ext);
    });
  }

  void addPattern(Pattern pattern, Set<Pattern> patterns) {
    patterns.add(RegExp(pattern, multiLine: true));
  }

  String patternSetToString(Set<Pattern> patterns) {
    return '{' +
        patterns.map((p) => '"${(p as RegExp).pattern}"').join(', ') +
        '}';
  }

  String stringSetToString(Set<String> set) {
    return '{' + set.map((s) => '"$s"').join(', ') + '}';
  }

  @override
  String toString() => 'FindSettings(archivesOnly: $archivesOnly'
      ', debug: $debug'
      ', excludeHidden: $excludeHidden'
      ', inArchiveExtensions: ${stringSetToString(inArchiveExtensions)}'
      ', inArchiveFilePatterns: ${patternSetToString(inArchiveFilePatterns)}'
      ', inDirPatterns: ${patternSetToString(inDirPatterns)}'
      ', inExtensions: ${stringSetToString(inExtensions)}'
      ', inFilePatterns: ${patternSetToString(inFilePatterns)}'
      ', inFileTypes: $inFileTypes'
      ', includeArchives: $includeArchives'
      ', listDirs: $listDirs'
      ', listFiles: $listFiles'
      ', outArchiveExtensions: ${stringSetToString(outArchiveExtensions)}'
      ', outArchiveFilePatterns: ${patternSetToString(outArchiveFilePatterns)}'
      ', outDirPatterns: ${patternSetToString(outDirPatterns)}'
      ', outExtensions: ${stringSetToString(outExtensions)}'
      ', outFilePatterns: ${patternSetToString(outFilePatterns)}'
      ', outFileTypes: $outFileTypes'
      ', paths: ${stringSetToString(paths)}'
      ', printUsage: $printUsage'
      ', printVersion: $printVersion'
      ', recursive: $recursive'
      ', verbose: $verbose'
      ')';
}

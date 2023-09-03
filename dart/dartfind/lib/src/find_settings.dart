import 'package:dartfind/src/file_types.dart';

enum SortBy {
  filePath,
  fileName,
  fileSize,
  fileType,
  lastMod,
}

extension SortByExtension on SortBy {
  String get name {
    switch (this) {
      case SortBy.fileName:
        return 'NAME';
      case SortBy.fileSize:
        return 'SIZE';
      case SortBy.fileType:
        return 'TYPE';
      case SortBy.lastMod:
        return 'LASTMOD';
      default:
        return 'PATH';
    }
  }
}

SortBy nameToSortBy(String name) {
  switch (name.trim().toLowerCase()) {
    case "name":
      return SortBy.fileName;
    case "size":
      return SortBy.fileSize;
    case "type":
      return SortBy.fileType;
    case "lastmod":
      return SortBy.lastMod;
    default:
      return SortBy.filePath;
  }
}

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

  int maxDepth = -1;
  DateTime? maxLastMod;
  int maxSize = 0;
  int minDepth = -1;
  DateTime? minLastMod;
  int minSize = 0;

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

  SortBy sortBy = SortBy.filePath;

  bool sortCaseInsensitive = false;
  bool sortDescending = false;
  bool verbose = false;

  bool? _needStat;

  void addExtensions(String exts, Set<String> extensions) {
    var extList = exts.split(',').where((ext) => ext.isNotEmpty).toList();
    addExtensionsList(extList, extensions);
  }

  void addExtensionsList(List<String> exts, Set<String> extensions) {
    for (var ext in exts) {
      extensions.add(ext);
    }
  }

  void addPattern(Pattern pattern, Set<Pattern> patterns) {
    patterns.add(RegExp(pattern.toString(), multiLine: true));
  }

  bool needStat() {
    _needStat ??= sortBy == SortBy.fileSize ||
        sortBy == SortBy.lastMod ||
        maxLastMod != null ||
        maxSize > 0 ||
        minLastMod != null ||
        minSize > 0;
    return _needStat!;
  }

  String dateTimeToString(DateTime? dt) {
    if (dt == null) {
      return '0';
    }
    return '"$dt"';
  }

  String patternSetToString(Set<Pattern> patterns) {
    return '{${patterns.map((p) => '"${(p as RegExp).pattern}"').join(', ')}}';
  }

  String stringSetToString(Set<String> set) {
    return '{${set.map((s) => '"$s"').join(', ')}}';
  }

  @override
  String toString() => 'FindSettings(archivesOnly=$archivesOnly'
      ', debug=$debug'
      ', excludeHidden=$excludeHidden'
      ', inArchiveExtensions=${stringSetToString(inArchiveExtensions)}'
      ', inArchiveFilePatterns=${patternSetToString(inArchiveFilePatterns)}'
      ', inDirPatterns=${patternSetToString(inDirPatterns)}'
      ', inExtensions=${stringSetToString(inExtensions)}'
      ', inFilePatterns=${patternSetToString(inFilePatterns)}'
      ', inFileTypes=$inFileTypes'
      ', includeArchives=$includeArchives'
      ', listDirs=$listDirs'
      ', listFiles=$listFiles'
      ', maxDepth=$maxDepth'
      ', maxLastMod=${dateTimeToString(maxLastMod)}'
      ', maxSize=$maxSize'
      ', minDepth=$minDepth'
      ', minLastMod=${dateTimeToString(minLastMod)}'
      ', minSize=$minSize'
      ', outArchiveExtensions=${stringSetToString(outArchiveExtensions)}'
      ', outArchiveFilePatterns=${patternSetToString(outArchiveFilePatterns)}'
      ', outDirPatterns=${patternSetToString(outDirPatterns)}'
      ', outExtensions=${stringSetToString(outExtensions)}'
      ', outFilePatterns=${patternSetToString(outFilePatterns)}'
      ', outFileTypes=$outFileTypes'
      ', paths=${stringSetToString(paths)}'
      ', printUsage=$printUsage'
      ', printVersion=$printVersion'
      ', recursive=$recursive'
      ', sortBy=${sortBy.name}'
      ', sortCaseInsensitive=$sortCaseInsensitive'
      ', sortDescending=$sortDescending'
      ', verbose=$verbose'
      ')';
}

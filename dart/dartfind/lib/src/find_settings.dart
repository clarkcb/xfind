import 'package:dartfind/src/file_types.dart';

const String filePathName = 'filepath';
const String fileNameName = 'filename';
const String nameName = 'name';
const String fileSizeName = 'filesize';
const String sizeName = 'size';
const String fileTypeName = 'filetype';
const String typeName = 'type';
const String lastModName = 'lastmod';

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
        return fileNameName;
      case SortBy.fileSize:
        return fileSizeName;
      case SortBy.fileType:
        return fileTypeName;
      case SortBy.lastMod:
        return lastModName;
      default:
        return filePathName;
    }
  }
}

SortBy nameToSortBy(String name) {
  switch (name.trim().toLowerCase()) {
    case fileNameName:
    case nameName:
      return SortBy.fileName;
    case fileSizeName:
    case sizeName:
      return SortBy.fileSize;
    case fileTypeName:
    case typeName:
      return SortBy.fileType;
    case lastModName:
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

  var inArchiveExtensions = <String>{};
  var inArchiveFilePatterns = <Pattern>{};
  var inDirPatterns = <Pattern>{};
  var inExtensions = <String>{};
  var inFilePatterns = <Pattern>{};

  var inFileTypes = <FileType>{};

  bool includeArchives = false;
  bool includeHidden = false;

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

  bool printDirs = false;
  bool printFiles = false;
  bool printUsage = false;
  bool printVersion = false;
  bool recursive = true;

  SortBy sortBy = SortBy.filePath;

  bool sortCaseInsensitive = false;
  bool sortDescending = false;
  bool verbose = false;

  bool? _needLastMod;
  bool? _needSize;

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

  bool needLastMod() {
    _needLastMod ??=
        sortBy == SortBy.lastMod || maxLastMod != null || minLastMod != null;
    return _needLastMod!;
  }

  bool needSize() {
    _needSize ??= sortBy == SortBy.fileSize || maxSize > 0 || minSize > 0;
    return _needSize!;
  }

  String dateTimeToString(DateTime? dt) {
    if (dt == null) {
      return '0';
    }
    return '"$dt"';
  }

  String fileTypeSetToString(Set<FileType> fileTypes) {
    return '[${fileTypes.map((ft) => ft.name).join(', ')}]';
  }

  String patternSetToString(Set<Pattern> patterns) {
    return '[${patterns.map((p) => '"${(p as RegExp).pattern}"').join(', ')}]';
  }

  String stringSetToString(Set<String> set) {
    return '[${set.map((s) => '"$s"').join(', ')}]';
  }

  @override
  String toString() => 'FindSettings(archivesOnly=$archivesOnly'
      ', debug=$debug'
      ', inArchiveExtensions=${stringSetToString(inArchiveExtensions)}'
      ', inArchiveFilePatterns=${patternSetToString(inArchiveFilePatterns)}'
      ', inDirPatterns=${patternSetToString(inDirPatterns)}'
      ', inExtensions=${stringSetToString(inExtensions)}'
      ', inFilePatterns=${patternSetToString(inFilePatterns)}'
      ', inFileTypes=${fileTypeSetToString(inFileTypes)}'
      ', includeArchives=$includeArchives'
      ', includeHidden=$includeHidden'
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
      ', outFileTypes=${fileTypeSetToString(outFileTypes)}'
      ', paths=${stringSetToString(paths)}'
      ', printDirs=$printDirs'
      ', printFiles=$printFiles'
      ', printUsage=$printUsage'
      ', printVersion=$printVersion'
      ', recursive=$recursive'
      ', sortBy=${sortBy.name}'
      ', sortCaseInsensitive=$sortCaseInsensitive'
      ', sortDescending=$sortDescending'
      ', verbose=$verbose'
      ')';
}

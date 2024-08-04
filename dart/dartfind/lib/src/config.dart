import 'dart:io' show Platform;
import 'package:path/path.dart' as path;

final String xFindPath = Platform.environment.containsKey('XFIND_PATH')
    ? Platform.environment['XFIND_PATH']!
    : path.join(Platform.environment['HOME']!, 'src', 'xfind');
final String sharedPath = path.join(xFindPath, 'shared');
final String fileTypesPath = path.join(sharedPath, 'filetypes.json');
final String findOptionsPath = path.join(sharedPath, 'findoptions.json');
final String xfindDb = path.join(sharedPath, 'xfind.db');

import 'dart:io' show Platform;

class FindConfig {
  String xFindConfigDir = "";
  String xFindPath = "";
  String fileTypesPath = "";
  String findOptionsPath = "";
  String defaultFindSettingsPath = "";

  FindConfig() {
    xFindConfigDir = Platform.environment.containsKey('XFIND_CONFIG_DIR')
        ? Platform.environment['XFIND_CONFIG_DIR']!
        : '${Platform.environment['HOME']!}/.config/xfind';

    xFindPath = Platform.environment.containsKey('XFIND_PATH')
        ? Platform.environment['XFIND_PATH']!
        : '${Platform.environment['HOME']!}/src/xfind';

    fileTypesPath = '$xFindPath/shared/filetypes.json';
    findOptionsPath = '$xFindPath/shared/findoptions.json';
    defaultFindSettingsPath = '${xFindConfigDir}/settings.json';
  }
}

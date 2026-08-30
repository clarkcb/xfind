#ifndef CPPFIND_CONFIG_H
#define CPPFIND_CONFIG_H

#include <string>

#define XFIND_REL_PATH "src/xfind"
#define FILE_TYPES_REL_PATH "shared/filetypes.json"
#define FIND_OPTIONS_REL_PATH "shared/findoptions.json"
#define XFIND_CONFIG_REL_DIR ".config/xfind"
#define DEFAULT_SETTINGS_REL_PATH ".config/xfind/settings.json"


namespace cppfind {
    struct FindConfig {
        std::string xfind_path;
        std::string file_types_path;
        std::string find_options_path;
        std::string default_find_settings_path;
    };

    FindConfig get_find_config();

    std::string xfind_config_dir();
    std::string xfind_path();
    std::string default_find_settings_path();
}

#endif // CPPFIND_CONFIG_H

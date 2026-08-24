#ifndef CPPFIND_CONFIG_H
#define CPPFIND_CONFIG_H

#include <string>

#define FILE_TYPES_REL_PATH "shared/filetypes.json"
#define FIND_OPTIONS_REL_PATH "shared/findoptions.json"
#define DEFAULT_SETTINGS_REL_PATH ".config/xfind/settings.json"


namespace cppfind {
    struct FindConfig {
        std::string xfind_path;
        std::string file_types_path;
        std::string find_options_path;
        std::string default_find_settings_path;
    };

    FindConfig get_find_config();

    std::string xfindpath();
    std::string default_find_settings_path();
}

#endif // CPPFIND_CONFIG_H

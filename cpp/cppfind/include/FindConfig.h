#ifndef CPPFIND_CONFIG_H
#define CPPFIND_CONFIG_H

#include <string>

#define FILE_TYPES_REL_PATH "shared/filetypes.json"
#define FIND_OPTIONS_REL_PATH "shared/findoptions.json"
#define DEFAULT_SETTINGS_REL_PATH ".config/xfind/settings.json"


namespace cppfind {
    std::string xfindpath();
}

#endif // CPPFIND_CONFIG_H

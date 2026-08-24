#include "FindConfig.h"

namespace cppfind {
    std::string xfindpath() {
        std::string xfindpath = std::getenv("XFIND_PATH");
        if (xfindpath.empty()) {
            const std::string home = std::getenv("HOME");
            if (home.empty()) {
                // TODO: throw exception?
                return "";
            }
            // TODO: make this cross-platform
            return home + "/src/xfind";
        }
        return xfindpath;
    }

    std::string default_find_settings_path() {
        const std::string home = std::getenv("HOME");
        if (home.empty()) {
            // TODO: throw exception?
            return "";
        }
        // TODO: make this cross-platform
        return home + "/" + DEFAULT_SETTINGS_REL_PATH;
    }

    FindConfig get_find_config() {
        const auto xfind_path = xfindpath();
        const auto file_types_path = xfind_path + "/" + FILE_TYPES_REL_PATH;
        const auto find_options_path = xfind_path + "/" + FIND_OPTIONS_REL_PATH;
        const std::string home_path = std::getenv("HOME");
        const auto default_find_settings_path = home_path + "/" + DEFAULT_SETTINGS_REL_PATH;

        return {
            xfind_path,
            file_types_path,
            find_options_path,
            default_find_settings_path
        };
    }
}

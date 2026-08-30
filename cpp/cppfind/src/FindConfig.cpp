#include "FindConfig.h"

namespace cppfind {
    std::string xfind_config_dir() {
        const char* xf_config_dir = std::getenv("XFIND_CONFIG_DIR");
        if (xf_config_dir == nullptr) {
            const char* home = std::getenv("HOME");
            if (home == nullptr) {
                // TODO: throw exception?
                return "";
            }
            // TODO: make this cross-platform
            return std::string(home) + "/" + XFIND_CONFIG_REL_DIR;
        }
        return xf_config_dir;
    }

    std::string xfind_path() {
        const char* xf_path = std::getenv("XFIND_PATH");
        if (xf_path == nullptr) {
            const char* home = std::getenv("HOME");
            if (home == nullptr) {
                // TODO: throw exception?
                return "";
            }
            // TODO: make this cross-platform
            return std::string(home) + XFIND_REL_PATH;
        }
        return xf_path;
    }

    std::string default_find_settings_path() {
        const std::string xf_config_dir = xfind_config_dir();
        return xf_config_dir + "/settings.json";
    }

    FindConfig get_find_config() {
        const auto xf_path = xfind_path();
        const auto file_types_path = xf_path + "/" + FILE_TYPES_REL_PATH;
        const auto find_options_path = xf_path + "/" + FIND_OPTIONS_REL_PATH;
        const auto find_settings_path = default_find_settings_path();

        return {
            xf_path,
            file_types_path,
            find_options_path,
            find_settings_path
        };
    }
}

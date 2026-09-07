#include "FindConfig.h"

namespace cppfind {
    std::string get_xfind_path() {
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

    std::string get_file_types_path() {
        const std::string xf_path = get_xfind_path();
        return xf_path + "/" + FILE_TYPES_REL_PATH;
    }

    std::string get_find_options_path() {
        const std::string xf_path = get_xfind_path();
        return xf_path + "/" + FIND_OPTIONS_REL_PATH;
    }

    std::string get_xfind_config_dir() {
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

    std::string get_default_find_settings_path() {
        const std::string xf_config_dir = get_xfind_config_dir();
        return xf_config_dir + "/settings.json";
    }

    FindConfig::FindConfig() :
        m_xfind_path(get_xfind_path()),
        m_file_types_path(get_file_types_path()),
        m_find_options_path(get_find_options_path()),
        m_default_find_settings_path(get_default_find_settings_path()) {
    }

    std::string FindConfig::xfind_path() const {
        return m_xfind_path;
    }

    std::string FindConfig::file_types_path() const {
        return m_file_types_path;
    }

    std::string FindConfig::find_options_path() const {
        return m_find_options_path;
    }

    std::string FindConfig::default_find_settings_path() const {
        return m_default_find_settings_path;
    }
}

#ifndef CPPFIND_CONFIG_H
#define CPPFIND_CONFIG_H

#include <string>

#define XFIND_REL_PATH "src/xfind"
#define FILE_TYPES_REL_PATH "shared/filetypes.json"
#define FIND_OPTIONS_REL_PATH "shared/findoptions.json"
#define XFIND_CONFIG_REL_DIR ".config/xfind"

namespace cppfind {
    class FindConfig {
    public:
        FindConfig();
        [[nodiscard]] std::string xfind_path() const;
        [[nodiscard]] std::string file_types_path() const;
        [[nodiscard]] std::string find_options_path() const;
        [[nodiscard]] std::string default_find_settings_path() const;

    private:
        std::string m_xfind_path;
        std::string m_file_types_path;
        std::string m_find_options_path;
        std::string m_default_find_settings_path;
    };
}

#endif // CPPFIND_CONFIG_H

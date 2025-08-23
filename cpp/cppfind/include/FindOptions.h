#ifndef CPPFIND_FINDOPTIONS_H
#define CPPFIND_FINDOPTIONS_H

#include <filesystem>
#include <functional>
#include <unordered_map>
#include <vector>

#include "rapidjson/document.h"

#include "FindOption.h"
#include "FindSettings.h"

namespace cppfind {
    class FindOptions {
    public:
        FindOptions();
        FindSettings settings_from_args(int &argc, char **argv);
        void update_settings_from_args(FindSettings& settings, int &argc, char **argv);
        void update_settings_from_file(FindSettings& settings, const std::filesystem::path& file_path);
        void update_settings_from_json(FindSettings& settings, std::string_view json_str);
        void usage();
        std::string get_usage_string();

    private:
        std::unordered_map<std::string, std::function<void(bool, FindSettings&)>> m_bool_arg_map;
        std::unordered_map<std::string, std::function<void(int, FindSettings&)>> m_int_arg_map;
        std::unordered_map<std::string, std::function<void(uint64_t, FindSettings&)>> m_long_arg_map;
        std::unordered_map<std::string, std::function<void(std::string&, FindSettings&)>> m_str_arg_map;
        std::unordered_map<std::string, std::string> m_arg_name_map;
        std::vector<FindOption> m_options;
        void load_options();
        void update_settings_from_document(FindSettings& settings, rapidjson::Document& document);

    };
}

#endif // CPPFIND_FINDOPTIONS_H

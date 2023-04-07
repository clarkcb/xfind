#ifndef CPPFIND_FINDOPTIONS_H
#define CPPFIND_FINDOPTIONS_H

#include <unordered_map>
#include <vector>
#include "rapidjson/document.h"
#include "FindOption.h"
#include "FindSettings.h"

using namespace rapidjson;

namespace cppfind {
    class FindOptions {
    private:
        std::unordered_map<std::string, std::function<void(std::string&, FindSettings*)>> m_str_arg_map;
        std::unordered_map<std::string, std::function<void(bool, FindSettings*)>> m_bool_arg_map;
        // std::unordered_map<std::string, std::function<void(unsigned int, FindSettings*)>> m_int_arg_map;
        // std::unordered_map<std::string, std::function<void(std::string&, FindSettings*)>> m_str_arg_map;
        std::unordered_map<std::string, std::string> m_long_arg_map;
        std::vector<FindOption*> m_options;
        void load_options();
        void settings_from_file(std::string& filepath, FindSettings* ss);
        void settings_from_document(Document* document, FindSettings* settings);

    public:
        FindOptions();
        FindSettings* settings_from_args(int &argc, char **argv);
        void settings_from_json(std::string& json, FindSettings* settings);
        void usage();
        std::string get_usage_string();
    };
}

#endif //CPPFIND_FINDOPTIONS_H

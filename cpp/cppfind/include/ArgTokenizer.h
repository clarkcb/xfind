#ifndef CPPFIND_ARGTOKENIZER_H
#define CPPFIND_ARGTOKENIZER_H

#include <filesystem>
#include <unordered_map>
#include <vector>

#include "rapidjson/document.h"

#include "ArgToken.h"

#define ARG_TOKEN_TYPE_BOOL 0
#define ARG_TOKEN_TYPE_STR  1
#define ARG_TOKEN_TYPE_INT  2
#define ARG_TOKEN_TYPE_LONG 3

namespace cppfind {
    class ArgTokenizer {
    public:
        ArgTokenizer();
        ArgTokenizer(const std::unordered_map<std::string, std::string> &bool_map,
                     const std::unordered_map<std::string, std::string> &str_map,
                     const std::unordered_map<std::string, std::string> &int_map,
                     const std::unordered_map<std::string, std::string> &long_map);
        [[nodiscard]] std::vector<ArgToken> tokenize_args(int &argc, char **argv) const;
        [[nodiscard]] std::vector<ArgToken> tokenize_json(std::string_view json) const;
        [[nodiscard]] std::vector<ArgToken> tokenize_file(const std::filesystem::path& file_path) const;

    private:
        std::unordered_map<std::string, std::string> m_bool_map;
        std::unordered_map<std::string, std::string> m_str_map;
        std::unordered_map<std::string, std::string> m_int_map;
        std::unordered_map<std::string, std::string> m_long_map;
        std::vector<ArgToken> tokenize_document(rapidjson::Document& document) const;

    };
}

#endif // CPPFIND_ARGTOKENIZER_H

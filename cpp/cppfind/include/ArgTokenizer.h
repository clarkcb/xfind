#ifndef CPPFIND_ARGTOKENIZER_H
#define CPPFIND_ARGTOKENIZER_H

#include <filesystem>
#include <unordered_map>
#include <vector>

#include "rapidjson/document.h"

#include "ArgToken.h"
#include "Option.h"

#define ARG_TOKEN_TYPE_UNKNOWN 0
#define ARG_TOKEN_TYPE_BOOL    1
#define ARG_TOKEN_TYPE_STR     2
#define ARG_TOKEN_TYPE_INT     3
#define ARG_TOKEN_TYPE_LONG    4

namespace cppfind {
    class ArgTokenizer {
    public:
        ArgTokenizer() = delete;
        explicit ArgTokenizer(const std::vector<std::unique_ptr<Option>>& options);
        [[nodiscard]] std::vector<ArgToken> tokenize_args(int argc, char **argv) const;
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

#include "ArgTokenizer.h"

#include <rapidjson/filereadstream.h>

#include "FileUtil.h"
#include "FindException.h"
#include "StringUtil.h"


namespace cppfind {
    ArgTokenizer::ArgTokenizer() = default;

    ArgTokenizer::ArgTokenizer(const std::unordered_map<std::string, std::string> &bool_map,
        const std::unordered_map<std::string, std::string> &str_map,
        const std::unordered_map<std::string, std::string> &int_map,
        const std::unordered_map<std::string, std::string> &long_map)
    : m_bool_map{bool_map},  m_str_map{str_map},  m_int_map{int_map},  m_long_map{long_map} {
    }

    std::vector<ArgToken> ArgTokenizer::tokenize_args(int &argc, char **argv) const {
        std::vector<ArgToken> arg_tokens{};
        std::deque<std::string> arg_deque;
        int i;

        for (i=1; i < argc; ++i) {
            arg_deque.emplace_back(argv[i]);
        }

        std::string next_arg;
        while (!arg_deque.empty()) {
            next_arg = arg_deque.front();
            arg_deque.pop_front();

            if (next_arg.rfind('-', 0) == 0) {
                std::deque<std::string> next_arg_deque;
                std::string next_val;
                if (next_arg.rfind("--", 0) == 0) {
                    if (next_arg.size() < 3) {
                        std::string msg{"Invalid option: " + next_arg};
                        msg.append(next_arg);
                        throw FindException(msg);
                    }
                    next_arg = next_arg.substr(2);
                    std::vector<std::string> parts = StringUtil::split_string(next_arg, '=');
                    if (!parts.empty()) {
                        next_arg = parts[0];
                    }
                    if (parts.size() > 1) {
                        next_val = parts[1];
                    }
                    next_arg_deque.emplace_back(next_arg);

                } else if (next_arg.size() > 1) {
                    next_arg = next_arg.substr(1);
                    for (const char & c : next_arg) {
                        std::string cs(1, c);
                        next_arg_deque.emplace_back(cs);
                    }

                } else {
                    std::string msg{"Invalid option: " + next_arg};
                    msg.append(next_arg);
                    throw FindException(msg);
                }

                while (!next_arg_deque.empty()) {
                    next_arg = next_arg_deque.front();
                    next_arg_deque.pop_front();

                    if (m_bool_map.contains(next_arg)) {
                        std::string long_arg = m_bool_map.at(next_arg);
                        arg_tokens.emplace_back(long_arg, ARG_TOKEN_TYPE_BOOL, true);
                    } else if (m_str_map.contains(next_arg)
                        || m_int_map.contains(next_arg)
                        || m_long_map.contains(next_arg)
                        || next_arg == "settings-file") {

                        if (next_val.empty()) {
                            if (arg_deque.empty()) {
                                std::string msg{"Missing value for option "};
                                msg.append(next_arg);
                                throw FindException(msg);
                            }
                            next_val = std::string(arg_deque.front());
                            arg_deque.pop_front();
                        }
                        if (m_str_map.contains(next_arg)) {
                            std::string long_arg = m_str_map.at(next_arg);
                            arg_tokens.emplace_back(long_arg, ARG_TOKEN_TYPE_STR, next_val);
                        } else if (m_int_map.contains(next_arg)) {
                            std::string long_arg = m_int_map.at(next_arg);
                            const int int_val = std::stoi(next_val);
                            arg_tokens.emplace_back(long_arg, ARG_TOKEN_TYPE_INT, int_val);
                        } else if (m_long_map.contains(next_arg)) {
                            std::string long_arg = m_long_map.at(next_arg);
                            const long long_val = std::stol(next_val);
                            arg_tokens.emplace_back(long_arg, ARG_TOKEN_TYPE_LONG, long_val);
                        } else {
                            // settings-file
                            arg_tokens.emplace_back(next_arg, ARG_TOKEN_TYPE_STR, next_val);
                        }
                    } else {
                        std::string msg{"Invalid option: "};
                        msg.append(next_arg);
                        throw FindException(msg);
                    }
                }
            } else {
                arg_tokens.emplace_back("path", ARG_TOKEN_TYPE_STR, next_arg);
            }
        }
        return arg_tokens;
    }

    std::vector<ArgToken> ArgTokenizer::tokenize_document(rapidjson::Document& document) const {
        assert(document.IsObject());

        std::vector<ArgToken> arg_tokens{};

        // Get the property names
        std::vector<std::string> names;
        for (rapidjson::Value::ConstMemberIterator it=document.MemberBegin(); it != document.MemberEnd(); ++it) {
            std::string name = it->name.GetString();
            names.push_back(name);
        }

        // Sort the names - produces consistent behavior across versions
        std::ranges::sort(names);

        // Iterate through names and values, validating values and applying to settings
        for (rapidjson::Value::ConstMemberIterator it=document.MemberBegin(); it != document.MemberEnd(); ++it) {
            if (std::string name = it->name.GetString(); m_bool_map.contains(name)) {
                if (it->value.IsBool()) {
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_BOOL, it->value.GetBool());
                } else {
                    std::string msg{"Invalid value for option: " + name};
                    throw FindException(msg);
                }
            } else if (m_str_map.contains(name) || name == "settings-file") {
                if (it->value.IsString()) {
                    auto s = std::string(it->value.GetString());
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_STR, s);
                } else if (it->value.IsArray()) {
                    const auto& arr = it->value.GetArray();
                    for (rapidjson::SizeType i = 0; i < arr.Size(); ++i) {
                        if (arr[i].IsString()) {
                            auto s = std::string(arr[i].GetString());
                            arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_STR, s);
                        } else {
                            std::string msg{"Invalid value for option: " + name};
                            throw FindException(msg);
                        }
                    }
                } else {
                    std::string msg{"Invalid value for option: " + name};
                    throw FindException(msg);
                }
            } else if (m_int_map.contains(name)) {
                if (it->value.IsInt()) {
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_INT, it->value.GetInt());
                } else if (it->value.IsUint()) {
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_INT, it->value.GetUint());
                } else if (it->value.IsInt64()) {
                    const auto l = it->value.GetInt64();
                    const int i = 0 + l;
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_INT, i);
                } else if (it->value.IsUint64()) {
                    const auto l = it->value.GetUint64();
                    const int i = 0 + l;
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_INT, i);
                } else {
                    std::string msg{"Invalid value for option: " + name};
                    throw FindException(msg);
                }
            } else if (m_long_map.contains(name)) {
                if (it->value.IsInt()) {
                    const auto i = it->value.GetInt();
                    const long l = 0 + i;
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_LONG, l);
                } else if (it->value.IsUint()) {
                    const auto i = it->value.GetUint();
                    const long l = 0 + i;
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_LONG, l);
                } else if (it->value.IsInt64()) {
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_LONG, it->value.GetInt64());
                } else if (it->value.IsUint64()) {
                    arg_tokens.emplace_back(name, ARG_TOKEN_TYPE_LONG, it->value.GetUint64());
                } else {
                    std::string msg{"Invalid value for option: " + name};
                    throw FindException(msg);
                }
            } else {
                // Shouldn't be able to get here since we already checked names
                std::string msg{"Invalid option: " + name};
                throw FindException(msg);
            }
        }
        return arg_tokens;
    }

    std::vector<ArgToken> ArgTokenizer::tokenize_json(const std::string_view json) const {
        rapidjson::Document document;
        document.Parse(std::string{json}.c_str());
        return tokenize_document(document);
    }

    std::vector<ArgToken> ArgTokenizer::tokenize_file(const std::filesystem::path& file_path) const {
        std::filesystem::path expanded_path = FileUtil::expand_path(file_path);
        if (!std::filesystem::exists(expanded_path)) {
            std::string msg{"Settings file not found: "};
            msg.append(file_path);
            throw FindException(msg);
        }

        if (file_path.extension() != ".json") {
            std::string msg{"Invalid settings file (must be JSON): "};
            msg.append(file_path);
            throw FindException(msg);
        }

        const uint64_t file_size = std::filesystem::file_size(expanded_path);
        // ~1MB, an arbitrary limit, but at least a limit
        assert(file_size <= 1024000);

        FILE *fp = fopen(expanded_path.c_str(), "r");

        char readBuffer[file_size];
        rapidjson::FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        rapidjson::Document document;
        document.ParseStream(is);
        fclose(fp);

        return tokenize_document(document);
    }

}

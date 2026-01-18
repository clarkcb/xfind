#include <algorithm>
#include <any>
#include <complex>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <typeinfo>

#include "rapidjson/filereadstream.h"

#include "ArgToken.h"
#include "FindConfig.h"
#include "FindException.h"
#include "FindOption.h"
#include "FindOptions.h"

namespace cppfind {
    FindOptions::FindOptions() : m_options(load_options()), m_arg_tokenizer(m_options) {
    }

    std::vector<std::unique_ptr<Option>> FindOptions::load_options() {
        std::vector<std::unique_ptr<Option>> options;
        auto find_options_path = std::filesystem::path(xfindpath()) / "shared/findoptions.json";

        if (!std::filesystem::exists(find_options_path)) {
            std::string msg{"Findoptions file not found: "};
            msg.append(find_options_path);
            throw FindException(msg);
        }

        const uint64_t file_size = std::filesystem::file_size(find_options_path);
        // current size is 5263, make sure it's not dramatically bigger than that
        if (file_size > 5300) {
            throw FindException("Invalid findoptions file");
        }

        FILE* fp = fopen(find_options_path.c_str(), "r");

        char readBuffer[file_size];
        rapidjson::FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        rapidjson::Document document;

        document.ParseStream(is);
        fclose(fp);

        assert(document.HasMember("findoptions"));
        const rapidjson::Value& find_options = document["findoptions"];
        assert(find_options.IsArray());
        for (rapidjson::SizeType i = 0; i < find_options.Size(); ++i) {
            const rapidjson::Value::ConstObject &find_option = find_options[i].GetObject();
            assert(find_option.HasMember("long"));
            const rapidjson::Value &longValue = find_option["long"];
            auto long_arg = std::string(longValue.GetString());
            m_arg_name_map[long_arg] = long_arg;

            std::string short_arg;
            if (find_option.HasMember("short")) {
                const rapidjson::Value &shortValue = find_option["short"];
                short_arg = std::string(shortValue.GetString());
                m_arg_name_map[short_arg] = long_arg;
            } else {
                short_arg = std::string("");
            }

            assert(find_option.HasMember("desc"));
            const rapidjson::Value &descValue = find_option["desc"];
            auto desc = std::string(descValue.GetString());

            int arg_type = ARG_TOKEN_TYPE_UNKNOWN;
            if (m_bool_arg_map.contains(long_arg)) {
                arg_type = ARG_TOKEN_TYPE_BOOL;
            } else if (m_str_arg_map.contains(long_arg)) {
                arg_type = ARG_TOKEN_TYPE_STR;
            } else if (m_int_arg_map.contains(long_arg)) {
                arg_type = ARG_TOKEN_TYPE_INT;
            } else if (m_long_arg_map.contains(long_arg)) {
                arg_type = ARG_TOKEN_TYPE_LONG;
            }
            options.push_back(std::make_unique<FindOption>(short_arg, long_arg, desc, arg_type));
        }
        return options;
    }

    void FindOptions::update_settings_from_arg_token(FindSettings& settings, const ArgToken& arg_token) {
        if (arg_token.token_type() == ARG_TOKEN_TYPE_BOOL) {
            if (m_bool_arg_map.contains(arg_token.name())) {
                if (arg_token.value().type() == typeid(bool)) {
                    m_bool_arg_map[arg_token.name()](std::any_cast<bool>(arg_token.value()), settings);
                } else {
                    std::string msg{"Invalid value for option: " + arg_token.name()};
                    throw FindException(msg);
                }
            } else {
                std::string msg{"Invalid option: " + arg_token.name()};
                throw FindException(msg);
            }
        } else if (arg_token.token_type() == ARG_TOKEN_TYPE_STR) {
            if (m_str_arg_map.contains(arg_token.name())) {
                if (arg_token.value().type() == typeid(std::string)) {
                    auto s = std::any_cast<std::string>(arg_token.value());
                    m_str_arg_map[arg_token.name()](s, settings);
                } else {
                    std::string msg{"Invalid value for option: " + arg_token.name()};
                    throw FindException(msg);
                }
            } else {
                std::string msg{"Invalid option: " + arg_token.name()};
                throw FindException(msg);
            }
        } else if (arg_token.token_type() == ARG_TOKEN_TYPE_INT) {
            if (m_int_arg_map.contains(arg_token.name())) {
                if (arg_token.value().type() == typeid(int) || arg_token.value().type() == typeid(unsigned)) {
                    auto i = std::any_cast<int>(arg_token.value());
                    m_int_arg_map[arg_token.name()](i, settings);
                } else {
                    std::string msg{"Invalid value for option: " + arg_token.name()};
                    throw FindException(msg);
                }
            } else {
                std::string msg{"Invalid option: " + arg_token.name()};
                throw FindException(msg);
            }
        } else if (arg_token.token_type() == ARG_TOKEN_TYPE_LONG) {
            if (m_long_arg_map.contains(arg_token.name())) {
                if (arg_token.value().type() == typeid(long)) {
                    auto l = std::any_cast<long>(arg_token.value());
                    m_long_arg_map[arg_token.name()](l, settings);
                } else {
                    std::string msg{"Invalid value for option: " + arg_token.name()};
                    throw FindException(msg);
                }
            } else {
                std::string msg{"Invalid option: " + arg_token.name()};
                throw FindException(msg);
            }
        }
    }

    void FindOptions::update_settings_from_arg_tokens(FindSettings& settings, const std::vector<ArgToken>& arg_tokens) {
        for (const auto& arg_token : arg_tokens) {
            update_settings_from_arg_token(settings, arg_token);
        }
    }

    void FindOptions::update_settings_from_args(FindSettings& settings, int argc, char **argv) {
        const auto arg_tokens = m_arg_tokenizer.tokenize_args(argc, argv);
        update_settings_from_arg_tokens(settings, arg_tokens);
    }

    FindSettings FindOptions::settings_from_args(int argc, char **argv) {
        auto settings = FindSettings();

        // set print_files to true since we are running the executable
        settings.print_files(true);

        update_settings_from_args(settings, argc, argv);

        return settings;
    }

    void FindOptions::update_settings_from_json(FindSettings& settings, const std::string_view json_str) {
        const auto arg_tokens = m_arg_tokenizer.tokenize_json(json_str);
        update_settings_from_arg_tokens(settings, arg_tokens);
    }

    void FindOptions::update_settings_from_file(FindSettings& settings, const std::filesystem::path& file_path) {
        const auto arg_tokens = m_arg_tokenizer.tokenize_file(file_path);
        update_settings_from_arg_tokens(settings, arg_tokens);
    }

    std::string FindOptions::get_usage_string() {
        std::string usage_string;
        usage_string.reserve(2930);
        usage_string += "\nUsage:\n cppfind [options] <path> [<path> ...]\n\nOptions:\n";

        std::vector<std::string> opt_strings{};
        std::vector<std::string> opt_descs{};

        auto options = std::vector<FindOption>{};
        for (auto const& o : m_options) {
            options.emplace_back(o->short_arg(), o->long_arg(), o->description(), o->arg_type());
        }

        auto sort_option_lambda = [](const FindOption& s1, const FindOption& s2) -> bool {
            return s1.sort_arg().compare(s2.sort_arg()) < 0;
        };
        std::ranges::sort(options, sort_option_lambda);

        unsigned long longest_len = 0;
        for (auto const& option : options) {
            std::string opt_string{};
            if (!option.short_arg().empty()) {
                opt_string.append("-").append(option.short_arg()).append(",");
            }
            opt_string.append("--").append(option.long_arg());
            if (opt_string.length() > longest_len) {
                longest_len = opt_string.length();
            }
            opt_strings.push_back(opt_string);
            opt_descs.push_back(option.description());
        }

        for (size_t i = 0; i < opt_strings.size(); ++i) {
            usage_string.append(" ");
            usage_string.append(opt_strings[i]);
            for (size_t j = 0; j <= longest_len - opt_strings[i].length() + 1; ++j) {
                usage_string.append(" ");
            }
            usage_string.append(opt_descs[i]);
            usage_string.append("\n");
        }
        return usage_string;
    }

    void FindOptions::usage() {
        const std::string usage_string{get_usage_string()};
        std::cout << usage_string << std::endl;
        exit(1);
    }
}

#include <algorithm>
#include <any>
#include <deque>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <boost/format.hpp>

#include "rapidjson/filereadstream.h"

#include "FindConfig.h"
#include "FindException.h"
#include "FindOptions.h"

#include "FileUtil.h"
#include "StringUtil.h"

namespace cppfind {
    FindOptions::FindOptions() {
        m_bool_arg_map = {
            {"archivesonly", [](const bool b, FindSettings& ss) -> void { ss.archives_only(b); }},
            {"debug", [](const bool b, FindSettings& ss) { ss.debug(b); }},
            {"excludearchives", [](const bool b, FindSettings& ss) { ss.include_archives(!b); }},
            {"excludehidden", [](const bool b, FindSettings& ss) { ss.include_hidden(!b); }},
            {"followsymlinks", [](const bool b, FindSettings& ss) { ss.follow_symlinks(b); }},
            {"help", [](const bool b, FindSettings& ss) { ss.print_usage(b); }},
            {"includearchives", [](const bool b, FindSettings& ss) { ss.include_archives(b); }},
            {"includehidden", [](const bool b, FindSettings& ss) { ss.include_hidden(b); }},
            {"nofollowsymlinks", [](const bool b, FindSettings& ss) { ss.follow_symlinks(!b); }},
            {"noprintdirs", [](const bool b, FindSettings& ss) { ss.print_dirs(!b); }},
            {"noprintfiles", [](const bool b, FindSettings& ss) { ss.print_files(!b); }},
            {"norecursive", [](const bool b, FindSettings& ss) { ss.recursive(!b); }},
            {"printdirs", [](const bool b, FindSettings& ss) { ss.print_dirs(b); }},
            {"printfiles", [](const bool b, FindSettings& ss) { ss.print_files(b); }},
            {"recursive", [](const bool b, FindSettings& ss) { ss.recursive(b); }},
            {"sort-ascending", [](const bool b, FindSettings& ss) { ss.sort_descending(!b); }},
            {"sort-caseinsensitive", [](const bool b, FindSettings& ss) { ss.sort_case_insensitive(b); }},
            {"sort-casesensitive", [](const bool b, FindSettings& ss) { ss.sort_case_insensitive(!b); }},
            {"sort-descending", [](const bool b, FindSettings& ss) { ss.sort_descending(b); }},
            {"verbose", [](const bool b, FindSettings& ss) { ss.verbose(b); }},
            {"version", [](const bool b, FindSettings& ss) { ss.print_version(b); }},
        };

        m_int_arg_map = {
            {"maxdepth", [](const int i, FindSettings& ss) { ss.max_depth(i); }},
            {"mindepth", [](const int i, FindSettings& ss) { ss.min_depth(i); }},
        };

        m_long_arg_map = {
            {"maxsize", [](const uint64_t lng, FindSettings& ss) { ss.max_size(lng); }},
            {"minsize", [](const uint64_t lng, FindSettings& ss) { ss.min_size(lng); }},
        };

        m_str_arg_map = {
            {"in-archiveext", [](const std::string& s, FindSettings& ss) { ss.add_in_archive_extension(s); }},
            {"in-archivefilepattern", [](const std::string& s, FindSettings& ss) { ss.add_in_archive_file_pattern(s); }},
            {"in-dirpattern", [](const std::string& s, FindSettings& ss) { ss.add_in_dir_pattern(s); }},
            {"in-ext", [](const std::string& s, FindSettings& ss) { ss.add_in_extension(s); }},
            {"in-filepattern", [](const std::string& s, FindSettings& ss) { ss.add_in_file_pattern(s); }},
            {"in-filetype", [](const std::string& s, FindSettings& ss) { ss.add_in_file_type(FileTypes::from_name(s)); }},
            {"maxlastmod", [](const std::string& s, FindSettings& ss) { ss.max_last_mod(StringUtil::date_str_to_long(s)); }},
            {"minlastmod", [](const std::string& s, FindSettings& ss) { ss.min_last_mod(StringUtil::date_str_to_long(s)); }},
            {"out-archiveext", [](const std::string& s, FindSettings& ss) { ss.add_out_archive_extension(s); }},
            {"out-archivefilepattern", [](const std::string& s, FindSettings& ss) { ss.add_out_archive_file_pattern(s); }},
            {"out-dirpattern", [](const std::string& s, FindSettings& ss) { ss.add_out_dir_pattern(s); }},
            {"out-ext", [](const std::string& s, FindSettings& ss) { ss.add_out_extension(s); }},
            {"out-filepattern", [](const std::string& s, FindSettings& ss) { ss.add_out_file_pattern(s); }},
            {"out-filetype", [](const std::string& s, FindSettings& ss) { ss.add_out_file_type(FileTypes::from_name(s)); }},
            {"path", [](const std::string& s, FindSettings& ss) { ss.add_path(s); }},
            {"settings-file", [this](const std::string& s, FindSettings& ss) { this->update_settings_from_file(s, ss); }},
            {"sort-by", [](const std::string& s, FindSettings& ss) { ss.sort_by(FindSettings::sort_by_from_name(s)); }}
        };

        load_options();
    }

    void FindOptions::load_options() {
        auto find_options_path = std::filesystem::path(xfindpath()) / "shared/findoptions.json";

        if (!std::filesystem::exists(find_options_path)) {
            std::string msg{"Findoptions file not found: "};
            msg.append(find_options_path);
            throw FindException(msg);
        }

        uint64_t file_size = std::filesystem::file_size(find_options_path);
        // current size is 4732, make sure it's not dramatically bigger than that
        if (file_size > 5120) {
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

            auto option = FindOption(short_arg, long_arg, desc);
            m_options.push_back(std::move(option));
        }
    }

    // TODO: try using https://github.com/CLIUtils/CLI11 for CLI arg parsing
    FindSettings FindOptions::settings_from_args(int &argc, char **argv) {
        auto settings = FindSettings();

        // set print_files to true since we are running the executable
        settings.print_files(true);

        std::deque<std::string> arg_deque;
        unsigned int i;

        for (i=1; i < argc; ++i) {
            arg_deque.emplace_back(argv[i]);
        }

        std::string next_arg;
        while (!arg_deque.empty()) {
            next_arg = arg_deque.front();
            arg_deque.pop_front();

            if (next_arg[0] == '-') {
                while (!next_arg.empty() && next_arg[0] == '-') {
                    next_arg = next_arg.substr(1);
                }

                if (m_arg_name_map.contains(next_arg)) {
                    if (auto long_arg = m_arg_name_map[next_arg];
                        m_bool_arg_map.contains(long_arg)) {
                        m_bool_arg_map[long_arg](true, settings);
                    } else if (m_str_arg_map.contains(long_arg)
                        || m_int_arg_map.contains(long_arg)
                        || m_long_arg_map.contains(long_arg)) {
                        if (arg_deque.empty()) {
                            std::string msg{"Missing value for option "};
                            msg.append(next_arg);
                            throw FindException(msg);
                        }
                        auto arg_val = std::string(arg_deque.front());
                        arg_deque.pop_front();
                        if (m_str_arg_map.contains(long_arg)) {
                            m_str_arg_map[long_arg](arg_val, settings);
                        } else if (m_int_arg_map.contains(long_arg)) {
                            const int int_val = std::stoi(arg_val);
                            m_int_arg_map[long_arg](int_val, settings);
                        } else if (m_long_arg_map.contains(long_arg)) {
                            const long long_val = std::stol(arg_val);
                            m_long_arg_map[long_arg](long_val, settings);
                        }
                    } else [[unlikely]] { // shouldn't be possible to get here
                        std::string msg{"Invalid option: "};
                        msg.append(next_arg);
                        throw FindException(msg);
                    }
                } else {
                    std::string msg{"Invalid option: "};
                    msg.append(next_arg);
                    throw FindException(msg);
                }
            } else {
                settings.add_path(next_arg);
            }
        }
        return settings;
    }

    void FindOptions::update_settings_from_document(rapidjson::Document& document, FindSettings& settings) {
        assert(document.IsObject());

        // create a map so we can sort the names
        std::vector<std::string> names;
        std::unordered_map<std::string, std::any> name_value_map;
        for (rapidjson::Value::ConstMemberIterator it=document.MemberBegin(); it != document.MemberEnd(); ++it) {
            std::string name = it->name.GetString();
            names.push_back(name);
            if (it->value.IsBool()) {
                name_value_map.insert(std::make_pair(name, it->value.GetBool()));
            } else if (it->value.IsString()) {
                auto s = std::string(it->value.GetString());
                name_value_map.insert(std::make_pair(name, s));
            } else if (it->value.IsInt()) {
                name_value_map.insert(std::make_pair(name, it->value.GetInt()));
            } else if (it->value.IsUint64()) {
                name_value_map.insert(std::make_pair(name, it->value.GetUint64()));
            } else if (it->value.IsArray()) {
                std::vector<std::string> vec;
                const auto& arr = it->value.GetArray();
                for (rapidjson::SizeType i = 0; i < arr.Size(); ++i) {
                    if (arr[i].IsString()) {
                        auto s = std::string(arr[i].GetString());
                        vec.emplace_back(s);
                    } else {
                        std::string msg{"Invalid value for option: " + name};
                        throw FindException(msg);
                    }
                }
                name_value_map.insert(std::make_pair(name, vec));
            } else {
                std::string msg{"Invalid value for option: " + name};
                throw FindException(msg);
            }
        }

        // Sort the names
        std::ranges::sort(names);

        for (const auto& name : names) {
            auto value = name_value_map[name];

            if (m_bool_arg_map.contains(name)) {
                if (value.type() == typeid(bool)) {
                    const bool b = std::any_cast<bool>(value);
                    // const bool b = value;
                    m_bool_arg_map[name](b, settings);
                } else {
                    std::string msg{"Invalid value for option: " + name};
                    throw FindException(msg);
                }
            } else if (m_str_arg_map.contains(name)) {
                if (value.type() == typeid(std::string)) {
                    auto s = std::any_cast<std::string>(value);
                    m_str_arg_map[name](s, settings);
                } else if (value.type() == typeid(std::vector<std::string>)) {
                    for (auto vec = std::any_cast<std::vector<std::string>>(value);
                         auto& s : vec) {
                        m_str_arg_map[name](s, settings);
                    }
                } else {
                    std::string msg{"Invalid value for option: " + name};
                    throw FindException(msg);
                }
            } else if (m_int_arg_map.contains(name)) {
                if (value.type() == typeid(int)) {
                    const int i = std::any_cast<int>(value);
                    m_int_arg_map[name](i, settings);
                } else if (value.type() == typeid(uint64_t)) {
                    const auto l = std::any_cast<uint64_t>(value);
                    const int i = 0 + l;
                    m_int_arg_map[name](i, settings);
                } else {
                    std::string msg{"Invalid value for option: " + name};
                    throw FindException(msg);
                }
            } else if (m_long_arg_map.contains(name)) {
                if (value.type() == typeid(int)) {
                    const int i = std::any_cast<int>(value);
                    const uint64_t l = 0 + i;
                    m_long_arg_map[name](l, settings);
                } else if (value.type() == typeid(uint64_t)) {
                    const auto l = std::any_cast<uint64_t>(value);
                    m_long_arg_map[name](l, settings);
                } else {
                    std::string msg{"Invalid value for option: " + name};
                    throw FindException(msg);
                }
            } else {
                const std::string msg = "Invalid option: " + name;
                throw FindException(msg);
            }
        }
    }

    void FindOptions::update_settings_from_json(const std::string_view json, FindSettings& settings) {
        rapidjson::Document document;
        document.Parse(std::string{json}.c_str());
        update_settings_from_document(document, settings);
    }

    void FindOptions::update_settings_from_file(const std::filesystem::path& file_path, FindSettings& settings) {
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

        update_settings_from_document(document, settings);
    }

    std::string FindOptions::get_usage_string() {
        std::string usage_string;
        usage_string.reserve(2930);
        usage_string += "\nUsage:\n cppfind [options] <path> [<path> ...]\n\nOptions:\n";

        std::vector<std::string> opt_strings{};
        std::vector<std::string> opt_descs{};

        auto sort_option_lambda = [](const FindOption& s1, const FindOption& s2) -> bool {
            return s1.sort_arg().compare(s2.sort_arg()) < 0;
        };
        std::ranges::sort(m_options, sort_option_lambda);

        unsigned long longest_len = 0;
        for (auto const& option : m_options) {
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

        const std::string format = std::string(" %1$-") + std::to_string(longest_len) + "s  %2$s\n";
        for (int i = 0; i < opt_strings.size(); ++i) {
            usage_string.append(boost::str(boost::format(format) % opt_strings[i] % opt_descs[i]));
        }
        return usage_string;
    }

    void FindOptions::usage() {
        const std::string usage_string{get_usage_string()};
        std::cout << usage_string << std::endl;
        exit(1);
    }
}

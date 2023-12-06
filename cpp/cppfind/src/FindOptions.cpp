#include <iostream>
#include <boost/format.hpp>
#include "rapidjson/filereadstream.h"

#include "config.h"
#include "FileUtil.h"
#include "FindException.h"
#include "FindOptions.h"
#include "StringUtil.h"

namespace cppfind {
    FindOptions::FindOptions() {
        m_str_arg_map = {
                {"in-archiveext", [](std::string& s, FindSettings& ss) { ss.add_in_archive_extension(s); }},
                {"in-archivefilepattern", [](std::string& s, FindSettings& ss) { ss.add_in_archive_file_pattern(s); }},
                {"in-dirpattern", [](std::string& s, FindSettings& ss) { ss.add_in_dir_pattern(s); }},
                {"in-ext", [](std::string& s, FindSettings& ss) { ss.add_in_extension(s); }},
                {"in-filepattern", [](std::string& s, FindSettings& ss) { ss.add_in_file_pattern(s); }},
                {"in-filetype", [](std::string& s, FindSettings& ss) { ss.add_in_file_type(FileTypes::from_name(s)); }},
                {"maxdepth", [](std::string& s, FindSettings& ss) { ss.max_depth(std::stoi(s)); }},
                {"maxlastmod", [](std::string& s, FindSettings& ss) { ss.max_last_mod(StringUtil::date_str_to_long(s)); }},
                {"maxsize", [](std::string& s, FindSettings& ss) { ss.max_size(std::stol(s)); }},
                {"mindepth", [](std::string& s, FindSettings& ss) { ss.min_depth(std::stoi(s)); }},
                {"minlastmod", [](std::string& s, FindSettings& ss) { ss.min_last_mod(StringUtil::date_str_to_long(s)); }},
                {"minsize", [](std::string& s, FindSettings& ss) { ss.min_size(std::stol(s)); }},
                {"out-archiveext", [](std::string& s, FindSettings& ss) { ss.add_out_archive_extension(s); }},
                {"out-archivefilepattern", [](std::string& s, FindSettings& ss) { ss.add_out_archive_file_pattern(s); }},
                {"out-dirpattern", [](std::string& s, FindSettings& ss) { ss.add_out_dir_pattern(s); }},
                {"out-ext", [](std::string& s, FindSettings& ss) { ss.add_out_extension(s); }},
                {"out-filepattern", [](std::string& s, FindSettings& ss) { ss.add_out_file_pattern(s); }},
                {"out-filetype", [](std::string& s, FindSettings& ss) { ss.add_out_file_type(FileTypes::from_name(s)); }},
                {"path", [](std::string& s, FindSettings& ss) { ss.add_path(s); }},
//                {"settings-file", [this](std::string& s, FindSettings& ss) { this->settings_from_file(s, ss); }},
                {"sort-by", [this](std::string& s, FindSettings& ss) { ss.sort_by(FindSettings::sort_by_from_name(s)); }}
        };

        m_bool_arg_map = {
                {"archivesonly", [](bool b, FindSettings& ss) { ss.archives_only(b); }},
                {"debug", [](bool b, FindSettings& ss) { ss.debug(b); }},
                {"excludearchives", [](bool b, FindSettings& ss) { ss.include_archives(!b); }},
                {"excludehidden", [](bool b, FindSettings& ss) { ss.include_hidden(!b); }},
                {"help", [](bool b, FindSettings& ss) { ss.print_usage(b); }},
                {"includearchives", [](bool b, FindSettings& ss) { ss.include_archives(b); }},
                {"includehidden", [](bool b, FindSettings& ss) { ss.include_hidden(b); }},
                {"listdirs", [](bool b, FindSettings& ss) { ss.list_dirs(b); }},
                {"listfiles", [](bool b, FindSettings& ss) { ss.list_files(b); }},
                {"nolistfiles", [](bool b, FindSettings& ss) { ss.list_files(!b); }},
                {"norecursive", [](bool b, FindSettings& ss) { ss.recursive(!b); }},
                {"recursive", [](bool b, FindSettings& ss) { ss.recursive(b); }},
                {"sort-ascending", [](bool b, FindSettings& ss) { ss.sort_descending(!b); }},
                {"sort-caseinsensitive", [](bool b, FindSettings& ss) { ss.sort_case_insensitive(b); }},
                {"sort-casesensitive", [](bool b, FindSettings& ss) { ss.sort_case_insensitive(!b); }},
                {"sort-descending", [](bool b, FindSettings& ss) { ss.sort_descending(b); }},
                {"verbose", [](bool b, FindSettings& ss) { ss.verbose(b); }},
                {"version", [](bool b, FindSettings& ss) { ss.print_version(b); }},
        };

        load_options();
    }

    void FindOptions::load_options() {
        auto xfind_path = xfindpath();
        auto sub_path = "shared/findoptions.json";
        auto findoptions_path = FileUtil::join_path(xfind_path, sub_path);

        if (!FileUtil::file_exists(findoptions_path)) {
            std::string msg = "Findoptions file not found: ";
            msg.append(findoptions_path);
            throw FindException(msg);
        }

        uint64_t file_size = FileUtil::file_size(findoptions_path);
        FILE* fp = fopen(findoptions_path.c_str(), "r");

        char readBuffer[file_size];
        rapidjson::FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        rapidjson::Document document;

        document.ParseStream(is);
        fclose(fp);

        assert(document.HasMember("findoptions"));
        const rapidjson::Value& findoptions = document["findoptions"];
        assert(findoptions.IsArray());
        for (rapidjson::SizeType i = 0; i < findoptions.Size(); i++) {
            const rapidjson::Value::ConstObject &findoption = findoptions[i].GetObject();
            assert(findoption.HasMember("long"));
            const rapidjson::Value &longValue = findoption["long"];
            auto long_arg = std::string(longValue.GetString());
            m_long_arg_map[long_arg] = long_arg;

            std::string short_arg;
            if (findoption.HasMember("short")) {
                const rapidjson::Value &shortValue = findoption["short"];
                short_arg = std::string(shortValue.GetString());
                m_long_arg_map[short_arg] = long_arg;
            } else {
                short_arg = std::string("");
            }

            assert(findoption.HasMember("desc"));
            const rapidjson::Value &descValue = findoption["desc"];
            auto desc = std::string(descValue.GetString());

            auto option = FindOption(short_arg, long_arg, desc);
            m_options.push_back(option);
        }
    }

    FindSettings* FindOptions::settings_from_args(int &argc, char **argv) {
        auto settings = new FindSettings();

        // set list_files to true since we are running the executable
        settings->list_files(true);

        std::deque<std::string> arg_deque;
        unsigned int i;

        for (i=1; i < argc; i++) {
            auto arg = new std::string(argv[i]);
            arg_deque.push_back(*arg);
        }

        std::string next_arg;
        while (!arg_deque.empty()) {
            next_arg = std::string(arg_deque.front());
            arg_deque.pop_front();

            if (next_arg[0] == '-') {
                while (!next_arg.empty() && next_arg[0] == '-') {
                    next_arg = next_arg.substr(1);
                }

                auto long_arg_found = m_long_arg_map.find(next_arg);
                if (long_arg_found != m_long_arg_map.end()) {
                    auto long_arg = m_long_arg_map[next_arg];

                    auto bool_arg_found = m_bool_arg_map.find(long_arg);
                    auto coll_arg_found = m_str_arg_map.find(long_arg);

                    if (bool_arg_found != m_bool_arg_map.end()) {
                        m_bool_arg_map[long_arg](true, *settings);
                    } else if (coll_arg_found != m_str_arg_map.end()) {
                        if (arg_deque.empty()) {
                            std::string msg = "Missing value for option ";
                            msg.append(next_arg);
                            throw FindException(msg);
                        } else {
                            auto* arg_val = new std::string(arg_deque.front());
                            arg_deque.pop_front();
                            if (coll_arg_found != m_str_arg_map.end()) {
                                m_str_arg_map[long_arg](*arg_val, *settings);
                            }
                        }
                    } else { // shouldn't be possible to get here
                        std::string msg = "Invalid option: ";
                        msg.append(next_arg);
                        throw FindException(msg);
                    }
                } else {
                    std::string msg = "Invalid option: ";
                    msg.append(next_arg);
                    throw FindException(msg);
                }
            } else {
                settings->add_path(next_arg);
            }
        }
        return settings;
    }

    void FindOptions::settings_from_file(std::string& file_path, FindSettings& settings) {
        if (!FileUtil::file_exists(file_path)) {
            std::string msg = "Settings file not found: ";
            msg.append(file_path);
            throw FindException(msg);
        }

        uint64_t file_size = FileUtil::file_size(file_path);
        FILE *fp = fopen(file_path.c_str(), "r");

        char readBuffer[file_size];
        rapidjson::FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        rapidjson::Document document;
        document.ParseStream(is);
        fclose(fp);

        settings_from_document(document, settings);
    }

    void FindOptions::settings_from_json(std::string& json, FindSettings& settings) {
        rapidjson::Document document;
        document.Parse(json.c_str());
        settings_from_document(document, settings);
    }

    void FindOptions::settings_from_document(rapidjson::Document& document, FindSettings& settings) {
        assert(document.IsObject());

        for(rapidjson::Value::ConstMemberIterator it=document.MemberBegin(); it != document.MemberEnd(); it++) {
            std::string name = it->name.GetString();

            if (it->value.IsArray()) {
                assert(m_str_arg_map.find(name) != m_str_arg_map.end());
                const auto& arr = it->value.GetArray();
                for (rapidjson::SizeType i = 0; i < arr.Size(); i++) {
                    assert(arr[i].IsString());
                    auto s = std::string(arr[i].GetString());
                    m_str_arg_map[name](s, settings);
                }

            } else if (it->value.IsBool()) {
                assert(m_bool_arg_map.find(name) != m_bool_arg_map.end());
                bool b = it->value.GetBool();
                m_bool_arg_map[name](b, settings);

            } else if (it->value.IsString()) {
                auto s = std::string(it->value.GetString());
                if (m_str_arg_map.find(name) != m_str_arg_map.end()) {
                    m_str_arg_map[name](s, settings);
                } else {
                    std::string msg = "Invalid option: " + name;
                    throw FindException(msg);
                }
            }
        }
    }

    void FindOptions::usage() {
        std::string usage_string{get_usage_string()};
        std::cout << usage_string << std::endl;
        exit(1);
    }

    std::string FindOptions::get_usage_string() {
        std::string usage_string{"\nUsage:\n cppfind [options] <path> [<path> ...]\n\nOptions:\n"};

        std::vector<std::string> opt_strings{};
        std::vector<std::string> opt_descs{};

        auto sort_option_lambda = [](const FindOption& s1, const FindOption& s2) -> bool {
            return s1.sort_arg().compare(s2.sort_arg()) < 0;
        };
        std::sort(m_options.begin(), m_options.end(), sort_option_lambda);

        long longest_len = 0;
        for (auto const& option : m_options) {
            std::string opt_string{};
            const std::string short_arg = option.short_arg();
            if (!short_arg.empty()) {
                opt_string.append("-").append(short_arg).append(",");
            }
            opt_string.append("--").append(option.long_arg());
            if (opt_string.length() > longest_len) {
                longest_len = opt_string.length();
            }
            opt_strings.push_back(opt_string);
            opt_descs.push_back(option.description());
        }

        std::string format = std::string(" %1$-") + std::to_string(longest_len) + "s  %2$s\n";

        for (int i = 0; i < opt_strings.size(); i++) {
            usage_string.append(boost::str(boost::format(format) % opt_strings[i] % opt_descs[i]));
        }
        return usage_string;
    }

}

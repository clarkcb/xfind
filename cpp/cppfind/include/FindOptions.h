#ifndef CPPFIND_FINDOPTIONS_H
#define CPPFIND_FINDOPTIONS_H

#include <filesystem>
#include <functional>
#include <unordered_map>
#include <vector>

#include "rapidjson/document.h"

#include "ArgTokenizer.h"
#include "Option.h"
#include "FindSettings.h"
#include "StringUtil.h"

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
        std::unordered_map<std::string, std::function<void(bool, FindSettings&)>> m_bool_arg_map = {
            {"archivesonly", [](const bool b, FindSettings& ss) -> void { ss.archives_only(b); }},
            {"colorize", [](const bool b, FindSettings& ss) { ss.colorize(b); }},
            {"debug", [](const bool b, FindSettings& ss) { ss.debug(b); }},
            {"excludearchives", [](const bool b, FindSettings& ss) { ss.include_archives(!b); }},
            {"excludehidden", [](const bool b, FindSettings& ss) { ss.include_hidden(!b); }},
            {"followsymlinks", [](const bool b, FindSettings& ss) { ss.follow_symlinks(b); }},
            {"help", [](const bool b, FindSettings& ss) { ss.print_usage(b); }},
            {"includearchives", [](const bool b, FindSettings& ss) { ss.include_archives(b); }},
            {"includehidden", [](const bool b, FindSettings& ss) { ss.include_hidden(b); }},
            {"nocolorize", [](const bool b, FindSettings& ss) { ss.colorize(!b); }},
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

        std::unordered_map<std::string, std::function<void(int, FindSettings&)>> m_int_arg_map = {
            {"maxdepth", [](const int i, FindSettings& ss) { ss.max_depth(i); }},
            {"mindepth", [](const int i, FindSettings& ss) { ss.min_depth(i); }},
        };

        std::unordered_map<std::string, std::function<void(uint64_t, FindSettings&)>> m_long_arg_map = {
            {"maxsize", [](const uint64_t lng, FindSettings& ss) { ss.max_size(lng); }},
            {"minsize", [](const uint64_t lng, FindSettings& ss) { ss.min_size(lng); }},
        };

        std::unordered_map<std::string, std::function<void(std::string&, FindSettings&)>> m_str_arg_map = {
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
            {"settings-file", [this](const std::string& s, FindSettings& ss) { this->update_settings_from_file(ss, s); }},
            {"sort-by", [](const std::string& s, FindSettings& ss) { ss.sort_by(FindSettings::sort_by_from_name(s)); }}
        };

        std::unordered_map<std::string, std::string> m_arg_name_map = {
            {"path", "path"},
        };

        std::vector<std::unique_ptr<Option>> m_options;
        ArgTokenizer m_arg_tokenizer;
        std::vector<std::unique_ptr<Option>> load_options();
        void update_settings_from_arg_token(FindSettings& settings, const ArgToken& arg_tokens);
        void update_settings_from_arg_tokens(FindSettings& settings, const std::vector<ArgToken>& arg_tokens);
    };
}

#endif // CPPFIND_FINDOPTIONS_H

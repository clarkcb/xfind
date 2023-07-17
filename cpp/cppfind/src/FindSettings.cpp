#include <boost/algorithm/string/case_conv.hpp>
#include "common.h"
#include "StringUtil.h"
#include "FindSettings.h"

namespace cppfind {
    FindSettings::FindSettings() {
        m_in_archive_extensions = {};
        m_in_archive_file_patterns = {};
        m_in_dir_patterns = {};
        m_in_extensions = {};
        m_in_file_patterns = {};
        m_in_file_types = {};
        m_out_archive_extensions = {};
        m_out_archive_file_patterns = {};
        m_out_dir_patterns = {};
        m_out_extensions = {};
        m_out_file_patterns = {};
        m_out_file_types = {};
        m_paths = {};
        m_sort_by = SortBy::FILEPATH;
    }

    void FindSettings::add_pattern(const std::string& p, std::vector<RegexPattern*>* ps) {
        ps->push_back(new RegexPattern(p));
    }

    void FindSettings::add_extensions(const std::string& exts, std::vector<std::string>* extensions) {
        std::vector<std::string> xs = StringUtil::split_string(exts, ",");
        for (const auto& x : xs) {
            if (!x.empty()) {
                extensions->push_back(x);
            }
        }
    }

    void FindSettings::add_in_archive_extension(const std::string& ext) {
        add_extensions(ext, &m_in_archive_extensions);
    }

    void FindSettings::add_in_archive_file_pattern(const std::string& p) {
        add_pattern(p, &m_in_archive_file_patterns);
    }

    void FindSettings::add_in_dir_pattern(const std::string& p) {
        add_pattern(p, &m_in_dir_patterns);
    }

    void FindSettings::add_in_extension(const std::string& ext) {
        add_extensions(ext, &m_in_extensions);
    }

    void FindSettings::add_in_file_pattern(const std::string& p) {
        add_pattern(p, &m_in_file_patterns);
    }

    void FindSettings::add_in_file_type(const FileType filetype) {
        m_in_file_types.push_back(filetype);
    }

    void FindSettings::add_out_archive_extension(const std::string& ext) {
        add_extensions(ext, &m_out_archive_extensions);
    }

    void FindSettings::add_out_archive_file_pattern(const std::string& p) {
        add_pattern(p, &m_out_archive_file_patterns);
    }

    void FindSettings::add_out_dir_pattern(const std::string& p) {
        add_pattern(p, &m_out_dir_patterns);
    }

    void FindSettings::add_out_extension(const std::string& ext) {
        add_extensions(ext, &m_out_extensions);
    }

    void FindSettings::add_out_file_pattern(const std::string& p) {
        add_pattern(p, &m_out_file_patterns);
    }

    void FindSettings::add_out_file_type(const FileType file_type) {
        m_out_file_types.push_back(file_type);
    }

    void FindSettings::add_path(const std::string& p) {
        m_paths.push_back(p);
    }

    bool FindSettings::archives_only() const {
        return m_archives_only;
    }

    bool FindSettings::debug() const {
        return m_debug;
    }

    bool FindSettings::exclude_hidden() const {
        return m_exclude_hidden;
    }

    bool FindSettings::include_archives() const {
        return m_include_archives;
    }

    bool FindSettings::list_dirs() const {
        return m_list_dirs;
    }

    bool FindSettings::list_files() const {
        return m_list_files;
    }

    long FindSettings::max_last_mod() const {
        return m_max_last_mod;
    }

    long FindSettings::max_size() const {
        return m_max_size;
    }

    long FindSettings::min_last_mod() const {
        return m_min_last_mod;
    }

    long FindSettings::min_size() const {
        return m_min_size;
    }

    bool FindSettings::print_usage() const {
        return m_print_usage;
    }

    bool FindSettings::print_version() const {
        return m_print_version;
    }

    bool FindSettings::recursive() const {
        return m_recursive;
    }

    bool FindSettings::sort_case_insensitive() const {
        return m_sort_case_insensitive;
    }

    bool FindSettings::sort_descending() const {
        return m_sort_descending;
    }

    bool FindSettings::verbose() const {
        return m_verbose;
    }

    std::vector<std::string>* FindSettings::in_archive_extensions() {
        return &m_in_archive_extensions;
    }

    std::vector<RegexPattern*>* FindSettings::in_archive_file_patterns() {
        return &m_in_archive_file_patterns;
    }

    std::vector<RegexPattern*>* FindSettings::in_dir_patterns() {
        return &m_in_dir_patterns;
    }

    std::vector<std::string>* FindSettings::in_extensions() {
        return &m_in_extensions;
    }

    std::vector<RegexPattern*>* FindSettings::in_file_patterns() {
        return &m_in_file_patterns;
    }

    std::vector<FileType>* FindSettings::in_file_types() {
        return &m_in_file_types;
    }

    std::vector<std::string>* FindSettings::out_archive_extensions() {
        return &m_out_archive_extensions;
    }

    std::vector<RegexPattern*>* FindSettings::out_archive_file_patterns() {
        return &m_out_archive_file_patterns;
    }

    std::vector<RegexPattern*>* FindSettings::out_dir_patterns() {
        return &m_out_dir_patterns;
    }

    std::vector<std::string>* FindSettings::out_extensions() {
        return &m_out_extensions;
    }

    std::vector<RegexPattern*>* FindSettings::out_file_patterns() {
        return &m_out_file_patterns;
    }

    std::vector<FileType>* FindSettings::out_file_types() {
        return &m_out_file_types;
    }

    bool FindSettings::need_stat() {
        return m_sort_by == SortBy::FILESIZE
            || m_sort_by == SortBy::LASTMOD
            || m_max_last_mod != 0
            || m_min_last_mod != 0
            || m_max_size > 0
            || m_min_size > 0;
    }

    std::vector<std::string>* FindSettings::paths() {
        return &m_paths;
    }

    SortBy FindSettings::sort_by() {
        return m_sort_by;
    }

    void FindSettings::archives_only(const bool b) {
        m_archives_only = b;
        if (b) m_include_archives = b;
    }

    void FindSettings::debug(const bool b) {
        m_debug = b;
        if (b) m_verbose = b;
    }

    void FindSettings::exclude_hidden(const bool b) {
        m_exclude_hidden = b;
    }

    void FindSettings::include_archives(bool b) {
        m_include_archives = b;
    }

    void FindSettings::list_dirs(const bool b) {
        m_list_dirs = b;
    }

    void FindSettings::list_files(const bool b) {
        m_list_files = b;
    }

    void FindSettings::max_last_mod(const long max_last_mod) {
        m_max_last_mod = max_last_mod;
    }

    void FindSettings::max_size(const long max_size) {
        m_max_size = max_size;
    }

    void FindSettings::min_last_mod(const long min_last_mod) {
        m_min_last_mod = min_last_mod;
    }

    void FindSettings::min_size(const long min_size) {
        m_min_size = min_size;
    }

    void FindSettings::print_usage(const bool b) {
        m_print_usage = b;
    }

    void FindSettings::print_version(const bool b) {
        m_print_version = b;
    }

    void FindSettings::recursive(const bool b) {
        m_recursive = b;
    }

    void FindSettings::sort_by(const SortBy sort_by) {
        m_sort_by = sort_by;
    }

    void FindSettings::set_sort_by(const std::string& name) {
        sort_by(FindSettings::sort_by_from_name(name));
    }

    void FindSettings::sort_case_insensitive(const bool b) {
        m_sort_case_insensitive = b;
    }

    void FindSettings::sort_descending(const bool b) {
        m_sort_descending = b;
    }

    void FindSettings::verbose(const bool b) {
        m_verbose = b;
    }

    std::string FindSettings::bool_to_string(bool b) {
        return b ? "true" : "false";
    }

    std::string FindSettings::string_vector_to_string(std::vector<std::string>* ss) {
        std::string ss_string = "[";
        int count = 0;
        for (auto const& s : *ss) {
            if (count > 0) {
                ss_string.append(", ");
            }
            ss_string.append("\"").append(s).append("\"");
            count++;
        }
        ss_string.append("]");
        return ss_string;
    }

    std::string FindSettings::find_patterns_to_string(std::vector<RegexPattern*>* ps) {
        std::string ps_string = "[";
        int count = 0;
        for (auto const& p : *ps) {
            if (count > 0) {
                ps_string.append(", ");
            }
            ps_string.append("\"").append(p->pattern()).append("\"");
            count++;
        }
        ps_string.append("]");
        return ps_string;
    }

    std::string FindSettings::file_types_to_string(std::vector<FileType>* ts) {
        std::string ts_string = "[";
        int count = 0;
        for (auto const& t : *ts) {
            if (count > 0) {
                ts_string.append(", ");
            }
            ts_string.append("\"").append(FileTypes::to_name(t)).append("\"");
            count++;
        }
        ts_string.append("]");
        return ts_string;
    }

    SortBy FindSettings::sort_by_from_name(const std::string& name) {
        std::string uname = boost::to_upper_copy(name);
        if (uname == "PATH") {
            return SortBy::FILEPATH;
        }
        if (uname == "NAME") {
            return SortBy::FILENAME;
        }
        if (uname == "SIZE") {
            return SortBy::FILESIZE;
        }
        if (uname == "TYPE") {
            return SortBy::FILETYPE;
        }
        if (uname == "LASTMOD") {
            return SortBy::LASTMOD;
        }
        return SortBy::FILEPATH;
    }

    std::string FindSettings::sort_by_to_name(const SortBy sortby) {
        switch (sortby)
        {
            case SortBy::FILEPATH:
                return "FILEPATH";
            case SortBy::FILENAME:
                return "FILENAME";
            case SortBy::FILESIZE:
                return "FILESIZE";
            case SortBy::FILETYPE:
                return "FILETYPE";
            case SortBy::LASTMOD:
                return "LASTMOD";
            default:
                return "UNKNOWN";
        }
    }

    std::string FindSettings::string() {
        auto settings_str =
                std::string("FindSettings(")
                + "archives_only: " + bool_to_string(m_archives_only)
                + ", debug: " + bool_to_string(m_debug)
                + ", exclude_hidden: " + bool_to_string(m_exclude_hidden)
                + ", in_archive_extensions: " + string_vector_to_string(&m_in_archive_extensions)
                + ", in_archive_file_patterns: " + find_patterns_to_string(&m_in_archive_file_patterns)
                + ", in_dir_patterns: " + find_patterns_to_string(&m_in_dir_patterns)
                + ", in_extensions: " + string_vector_to_string(&m_in_extensions)
                + ", in_file_patterns: " + find_patterns_to_string(&m_in_file_patterns)
                + ", in_file_types: " + file_types_to_string(&m_in_file_types)
                + ", include_archives: " + bool_to_string(m_include_archives)
                + ", list_dirs: " + bool_to_string(m_list_dirs)
                + ", list_files: " + bool_to_string(m_list_files)
                + ", max_last_mod: \"" + long_to_datestr(m_max_last_mod) + "\""
                + ", max_size: " + std::to_string(m_max_size)
                + ", min_last_mod: \"" + long_to_datestr(m_min_last_mod) + "\""
                + ", min_size: " + std::to_string(m_min_size)
                + ", out_archive_extensions: " + string_vector_to_string(&m_out_archive_extensions)
                + ", out_archive_file_patterns: " + find_patterns_to_string(&m_out_archive_file_patterns)
                + ", out_dir_patterns: " + find_patterns_to_string(&m_out_dir_patterns)
                + ", out_extensions: " + string_vector_to_string(&m_out_extensions)
                + ", out_file_patterns: " + find_patterns_to_string(&m_out_file_patterns)
                + ", out_file_types: " + file_types_to_string(&m_out_file_types)
                + ", paths: " + string_vector_to_string(&m_paths)
                + ", print_usage: " + bool_to_string(m_print_usage)
                + ", print_version: " + bool_to_string(m_print_version)
                + ", recursive: " + bool_to_string(m_recursive)
                + ", sort_by: " + sort_by_to_name(m_sort_by)
                + ", sort_case_insensitive: " + bool_to_string(m_sort_case_insensitive)
                + ", sort_descending: " + bool_to_string(m_sort_descending)
                + ", verbose: " + bool_to_string(m_verbose)
                + ")";
        return settings_str;
    }
}

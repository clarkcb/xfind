
#include "FindSettings.h"
#include "StringUtil.h"


namespace cppfind {

    bool FindSettings::archives_only() const { return m_archives_only; }

    void FindSettings::archives_only(bool archives_only) {
        m_archives_only = archives_only;
    }

    bool FindSettings::debug() const {
        return m_debug;
    }

    void FindSettings::debug(bool debug) {
        m_debug = debug;
    }

    bool FindSettings::exclude_hidden() const {
        return m_exclude_hidden;
    }

    void FindSettings::exclude_hidden(bool exclude_hidden) {
        m_exclude_hidden = exclude_hidden;
    }

    std::set<std::string> FindSettings::in_archive_extensions() const {
        return m_in_archive_extensions;
    }

    void FindSettings::in_archive_extensions(const std::set<std::string>& in_archive_extensions) {
        m_in_archive_extensions = in_archive_extensions;
    }

    std::set<RegexPattern, RegexPatternCmp> FindSettings::in_archive_file_patterns() const {
        return m_in_archive_file_patterns;
    }

    std::set<RegexPattern, RegexPatternCmp> FindSettings::in_dir_patterns() const {
        return m_in_dir_patterns;
    }

    void FindSettings::in_dir_patterns(const std::set<RegexPattern, RegexPatternCmp>& in_dir_patterns) {
        m_in_dir_patterns = in_dir_patterns;
    }

    std::set<std::string> FindSettings::in_extensions() const {
        return m_in_extensions;
    }

    void FindSettings::in_extensions(const std::set<std::string>& in_extensions) {
        m_in_extensions = in_extensions;
    }

    std::set<RegexPattern, RegexPatternCmp> FindSettings::in_file_patterns() const {
        return m_in_file_patterns;
    }

    void FindSettings::in_file_patterns(const std::set<RegexPattern, RegexPatternCmp>& in_file_patterns) {
        m_in_file_patterns = in_file_patterns;
    }

    std::set<FileType> FindSettings::in_file_types() const {
        return m_in_file_types;
    }

    void FindSettings::in_file_types(const std::set<FileType>& in_file_types) {
        m_in_file_types = in_file_types;
    }

    bool FindSettings::include_archives() const {
        return m_include_archives;
    }

    void FindSettings::include_archives(bool include_archives) {
        m_include_archives = include_archives;
    }

    bool FindSettings::list_dirs() const {
        return m_list_dirs;
    }

    void FindSettings::list_dirs(bool list_dirs) {
        m_list_dirs = list_dirs;
    }

    bool FindSettings::list_files() const {
        return m_list_files;
    }

    void FindSettings::list_files(bool list_files) {
        m_list_files = list_files;
    }

    int FindSettings::max_depth() const {
        return m_max_depth;
    }

    void FindSettings::max_depth(int max_depth) {
        m_max_depth = max_depth;
    }

    long FindSettings::max_last_mod() const {
        return m_max_last_mod;
    }

    void FindSettings::max_last_mod(long max_last_mod) {
        m_max_last_mod = max_last_mod;
    }

    long FindSettings::max_size() const {
        return m_max_size;
    }

    void FindSettings::max_size(long max_size) {
        m_max_size = max_size;
    }

    int FindSettings::min_depth() const {
        return m_min_depth;
    }

    void FindSettings::min_depth(int min_depth) {
        m_min_depth = min_depth;
    }

    long FindSettings::min_last_mod() const {
        return m_min_last_mod;
    }

    void FindSettings::min_last_mod(long min_last_mod) {
        m_min_last_mod = min_last_mod;
    }

    long FindSettings::min_size() const {
        return m_min_size;
    }

    void FindSettings::min_size(long min_size) {
        m_min_size = min_size;
    }

    std::set<std::string> FindSettings::out_archive_extensions() const {
        return m_out_archive_extensions;
    }

    void FindSettings::out_archive_extensions(const std::set<std::string>& out_archive_extensions) {
        m_out_archive_extensions = out_archive_extensions;
    }

    std::set<RegexPattern, RegexPatternCmp> FindSettings::out_archive_file_patterns() const {
        return m_out_archive_file_patterns;
    }

    std::set<RegexPattern, RegexPatternCmp> FindSettings::out_dir_patterns() const {
        return m_out_dir_patterns;
    }

    void FindSettings::out_dir_patterns(const std::set<RegexPattern, RegexPatternCmp>& out_dir_patterns) {
        m_out_dir_patterns = out_dir_patterns;
    }

    std::set<std::string> FindSettings::out_extensions() const {
        return m_out_extensions;
    }

    void FindSettings::out_extensions(const std::set<std::string>& out_extensions) {
        m_out_extensions = out_extensions;
    }

    std::set<RegexPattern, RegexPatternCmp> FindSettings::out_file_patterns() const {
        return m_out_file_patterns;
    }

    void FindSettings::out_file_patterns(const std::set<RegexPattern, RegexPatternCmp>& out_file_patterns) {
        m_out_file_patterns = out_file_patterns;
    }

    std::set<FileType> FindSettings::out_file_types() const {
        return m_out_file_types;
    }

    void FindSettings::out_file_types(const std::set<FileType>& out_file_types) {
        m_out_file_types = out_file_types;
    }

    bool FindSettings::print_usage() const {
        return m_print_usage;
    }

    void FindSettings::print_usage(bool print_usage) {
        m_print_usage = print_usage;
    }

    bool FindSettings::print_version() const {
        return m_print_version;
    }

    void FindSettings::print_version(bool print_version) {
        m_print_version = print_version;
    }

    bool FindSettings::recursive() const {
        return m_recursive;
    }

    void FindSettings::recursive(bool recursive) {
        m_recursive = recursive;
    }

    std::set<std::string> FindSettings::paths() const {
        return m_paths;
    }

    void FindSettings::paths(const std::set<std::string>& paths) {
        m_paths = paths;
    }

    SortBy FindSettings::sort_by() const {
        return m_sort_by;
    }

    void FindSettings::sort_by(SortBy sort_by) {
        m_sort_by = sort_by;
    }

    bool FindSettings::sort_case_insensitive() const {
        return m_sort_case_insensitive;
    }

    void FindSettings::sort_case_insensitive(bool sort_case_insensitive) {
        m_sort_case_insensitive = sort_case_insensitive;
    }

    bool FindSettings::sort_descending() const {
        return m_sort_descending;
    }

    void FindSettings::sort_descending(bool sort_descending) {
        m_sort_descending = sort_descending;
    }

    bool FindSettings::verbose() const {
        return m_verbose;
    }

    void FindSettings::verbose(bool verbose) {
        m_verbose = verbose;
    }

    void FindSettings::add_in_archive_extension(const std::string& ext) {
        add_extensions(ext, m_in_archive_extensions);
    }

    void FindSettings::add_in_archive_file_pattern(const std::string& p) {
        add_pattern(p, m_in_archive_file_patterns);
    }

    void FindSettings::add_in_dir_pattern(const std::string& p) {
        add_pattern(p, m_in_dir_patterns);
    }

    void FindSettings::add_in_extension(const std::string& ext) {
        add_extensions(ext, m_in_extensions);
    }

    void FindSettings::add_in_file_pattern(const std::string& p) {
        add_pattern(p, m_in_file_patterns);
    }

    void FindSettings::add_in_file_type(const FileType file_type) {
        m_in_file_types.emplace(file_type);
    }

    void FindSettings::add_out_archive_extension(const std::string& ext) {
        add_extensions(ext, m_out_archive_extensions);
    }

    void FindSettings::add_out_archive_file_pattern(const std::string& p) {
        add_pattern(p, m_out_archive_file_patterns);
    }

    void FindSettings::add_out_dir_pattern(const std::string& p) {
        add_pattern(p, m_out_dir_patterns);
    }

    void FindSettings::add_out_extension(const std::string& ext) {
        add_extensions(ext, m_out_extensions);
    }

    void FindSettings::add_out_file_pattern(const std::string& p) {
        add_pattern(p, m_out_file_patterns);
    }

    void FindSettings::add_out_file_type(const FileType file_type) {
        m_out_file_types.emplace(file_type);
    }

    void FindSettings::add_path(const std::string& p) {
        m_paths.emplace(p);
    }

    void FindSettings::add_pattern(const std::string& p, std::set<RegexPattern, RegexPatternCmp>& ps) {
        ps.emplace(p);
    }

    void FindSettings::add_extensions(const std::string& exts, std::set<std::string>& extensions) {
        std::vector<std::string> xs = StringUtil::split_string(exts, ",", true);
        for (const auto& x : xs) {
            if (!x.empty()) {
                extensions.emplace(x);
            }
        }
    }

    std::string FindSettings::find_patterns_to_string(std::set<RegexPattern, RegexPatternCmp>& patterns) {
        std::string ps_string = "[";
        int count = 0;
        for (auto const& p : patterns) {
            if (count > 0) {
                ps_string.append(", ");
            }
            ps_string.append("\"").append(p.pattern()).append("\"");
            count++;
        }
        ps_string.append("]");
        return ps_string;
    }

    std::string FindSettings::file_types_to_string(std::set<FileType>& types) {
        std::string ts_string = "[";
        int count = 0;
        for (auto const& t : types) {
            if (count > 0) {
                ts_string.append(", ");
            }
            ts_string.append("\"").append(FileTypes::to_name(t)).append("\"");
            count++;
        }
        ts_string.append("]");
        return ts_string;
    }

    bool FindSettings::need_stat() const {
        return m_sort_by == SortBy::FILESIZE
               || m_sort_by == SortBy::LASTMOD
               || m_max_last_mod != 0
               || m_min_last_mod != 0
               || m_max_size > 0
               || m_min_size > 0;
    }

    SortBy FindSettings::sort_by_from_name(const std::string& name) {
        std::string uname{name};
        std::transform(uname.begin(), uname.end(), uname.begin(),
                       [](unsigned char c) { return std::toupper(c); });
        if (uname == "PATH" || uname == "FILEPATH") {
            return SortBy::FILEPATH;
        }
        if (uname == "NAME" || uname == "FILENAME") {
            return SortBy::FILENAME;
        }
        if (uname == "SIZE" || uname == "FILESIZE") {
            return SortBy::FILESIZE;
        }
        if (uname == "TYPE" || uname == "FILETYPE") {
            return SortBy::FILETYPE;
        }
        if (uname == "LASTMOD") {
            return SortBy::LASTMOD;
        }
        return SortBy::FILEPATH;
    }

    std::string FindSettings::sort_by_to_name(const SortBy sort_by) {
        switch (sort_by)
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
                + "archives_only: " + StringUtil::bool_to_string(m_archives_only)
                + ", debug: " + StringUtil::bool_to_string(m_debug)
                + ", exclude_hidden: " + StringUtil::bool_to_string(m_exclude_hidden)
                + ", in_archive_extensions: " + StringUtil::string_set_to_string(m_in_archive_extensions)
                + ", in_archive_file_patterns: " + find_patterns_to_string(m_in_archive_file_patterns)
                + ", in_dir_patterns: " + find_patterns_to_string(m_in_dir_patterns)
                + ", in_extensions: " + StringUtil::string_set_to_string(m_in_extensions)
                + ", in_file_patterns: " + find_patterns_to_string(m_in_file_patterns)
                + ", in_file_types: " + file_types_to_string(m_in_file_types)
                + ", include_archives: " + StringUtil::bool_to_string(m_include_archives)
                + ", list_dirs: " + StringUtil::bool_to_string(m_list_dirs)
                + ", list_files: " + StringUtil::bool_to_string(m_list_files)
                + ", max_depth: " + std::to_string(m_max_depth)
                + ", max_last_mod: \"" + StringUtil::long_to_date_str(m_max_last_mod) + "\""
                + ", max_size: " + std::to_string(m_max_size)
                + ", min_depth: " + std::to_string(m_min_depth)
                + ", min_last_mod: \"" + StringUtil::long_to_date_str(m_min_last_mod) + "\""
                + ", min_size: " + std::to_string(m_min_size)
                + ", out_archive_extensions: " + StringUtil::string_set_to_string(m_out_archive_extensions)
                + ", out_archive_file_patterns: " + find_patterns_to_string(m_out_archive_file_patterns)
                + ", out_dir_patterns: " + find_patterns_to_string(m_out_dir_patterns)
                + ", out_extensions: " + StringUtil::string_set_to_string(m_out_extensions)
                + ", out_file_patterns: " + find_patterns_to_string(m_out_file_patterns)
                + ", out_file_types: " + file_types_to_string(m_out_file_types)
                + ", paths: " + StringUtil::string_set_to_string(m_paths)
                + ", print_usage: " + StringUtil::bool_to_string(m_print_usage)
                + ", print_version: " + StringUtil::bool_to_string(m_print_version)
                + ", recursive: " + StringUtil::bool_to_string(m_recursive)
                + ", sort_by: " + sort_by_to_name(m_sort_by)
                + ", sort_case_insensitive: " + StringUtil::bool_to_string(m_sort_case_insensitive)
                + ", sort_descending: " + StringUtil::bool_to_string(m_sort_descending)
                + ", verbose: " + StringUtil::bool_to_string(m_verbose)
                + ")";
        return settings_str;
    }
}

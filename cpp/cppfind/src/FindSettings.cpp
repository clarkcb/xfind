
#include <algorithm>
#include "FindSettings.h"

#include "StringUtil.h"

namespace cppfind {

    FindSettings::FindSettings() :
    m_archives_only{false},
    m_debug{false},
    m_follow_symlinks{false},
    m_include_archives{false},
    m_include_hidden{false},
    m_max_depth{-1},
    m_max_last_mod{0L},
    m_max_size{0L},
    m_min_depth{-1},
    m_min_last_mod{0L},
    m_min_size{0L},
    m_print_dirs{false},
    m_print_files{false},
    m_print_usage{false},
    m_print_version{false},
    m_recursive{true},
    m_sort_case_insensitive{false},
    m_sort_descending{false},
    m_verbose{false} {
    }

    bool FindSettings::archives_only() const { return m_archives_only; }

    void FindSettings::archives_only(const bool archives_only) {
        m_archives_only = archives_only;
        if (archives_only) m_include_archives = true;
    }

    bool FindSettings::debug() const {
        return m_debug;
    }

    void FindSettings::debug(const bool debug) {
        m_debug = debug;
        if (debug) m_verbose = true;
    }

    bool FindSettings::follow_symlinks() const {
        return m_follow_symlinks;
    }

    void FindSettings::follow_symlinks(const bool follow_symlinks) {
        m_follow_symlinks = follow_symlinks;
    }

    std::unordered_set<std::string> FindSettings::in_archive_extensions() const {
        return m_in_archive_extensions;
    }

    void FindSettings::in_archive_extensions(const std::unordered_set<std::string>& in_archive_extensions) {
        m_in_archive_extensions = in_archive_extensions;
    }

    std::unordered_set<RegexPattern, RegexPatternHash> FindSettings::in_archive_file_patterns() const {
        return m_in_archive_file_patterns;
    }

    std::unordered_set<RegexPattern, RegexPatternHash> FindSettings::in_dir_patterns() const {
        return m_in_dir_patterns;
    }

    void FindSettings::in_dir_patterns(const std::unordered_set<RegexPattern, RegexPatternHash>& in_dir_patterns) {
        m_in_dir_patterns = in_dir_patterns;
    }

    std::unordered_set<std::string> FindSettings::in_extensions() const {
        return m_in_extensions;
    }

    void FindSettings::in_extensions(const std::unordered_set<std::string>& in_extensions) {
        m_in_extensions = in_extensions;
    }

    std::unordered_set<RegexPattern, RegexPatternHash> FindSettings::in_file_patterns() const {
        return m_in_file_patterns;
    }

    void FindSettings::in_file_patterns(const std::unordered_set<RegexPattern, RegexPatternHash>& in_file_patterns) {
        m_in_file_patterns = in_file_patterns;
    }

    std::unordered_set<FileType> FindSettings::in_file_types() const {
        return m_in_file_types;
    }

    void FindSettings::in_file_types(const std::unordered_set<FileType>& in_file_types) {
        m_in_file_types = in_file_types;
    }

    bool FindSettings::include_archives() const {
        return m_include_archives;
    }

    void FindSettings::include_archives(const bool include_archives) {
        m_include_archives = include_archives;
    }

    bool FindSettings::include_hidden() const {
        return m_include_hidden;
    }

    void FindSettings::include_hidden(const bool include_hidden) {
        m_include_hidden = include_hidden;
    }

    int FindSettings::max_depth() const {
        return m_max_depth;
    }

    void FindSettings::max_depth(const int max_depth) {
        m_max_depth = max_depth;
    }

    long FindSettings::max_last_mod() const {
        return m_max_last_mod;
    }

    void FindSettings::max_last_mod(const long max_last_mod) {
        m_max_last_mod = max_last_mod;
    }

    uint64_t FindSettings::max_size() const {
        return m_max_size;
    }

    void FindSettings::max_size(const uint64_t max_size) {
        m_max_size = max_size;
    }

    int FindSettings::min_depth() const {
        return m_min_depth;
    }

    void FindSettings::min_depth(const int min_depth) {
        m_min_depth = min_depth;
    }

    long FindSettings::min_last_mod() const {
        return m_min_last_mod;
    }

    void FindSettings::min_last_mod(const long min_last_mod) {
        m_min_last_mod = min_last_mod;
    }

    uint64_t FindSettings::min_size() const {
        return m_min_size;
    }

    void FindSettings::min_size(const uint64_t min_size) {
        m_min_size = min_size;
    }

    std::unordered_set<std::string> FindSettings::out_archive_extensions() const {
        return m_out_archive_extensions;
    }

    void FindSettings::out_archive_extensions(const std::unordered_set<std::string>& out_archive_extensions) {
        m_out_archive_extensions = out_archive_extensions;
    }

    std::unordered_set<RegexPattern, RegexPatternHash> FindSettings::out_archive_file_patterns() const {
        return m_out_archive_file_patterns;
    }

    std::unordered_set<RegexPattern, RegexPatternHash> FindSettings::out_dir_patterns() const {
        return m_out_dir_patterns;
    }

    void FindSettings::out_dir_patterns(const std::unordered_set<RegexPattern, RegexPatternHash>& out_dir_patterns) {
        m_out_dir_patterns = out_dir_patterns;
    }

    std::unordered_set<std::string> FindSettings::out_extensions() const {
        return m_out_extensions;
    }

    void FindSettings::out_extensions(const std::unordered_set<std::string>& out_extensions) {
        m_out_extensions = out_extensions;
    }

    std::unordered_set<RegexPattern, RegexPatternHash> FindSettings::out_file_patterns() const {
        return m_out_file_patterns;
    }

    void FindSettings::out_file_patterns(const std::unordered_set<RegexPattern, RegexPatternHash>& out_file_patterns) {
        m_out_file_patterns = out_file_patterns;
    }

    std::unordered_set<FileType> FindSettings::out_file_types() const {
        return m_out_file_types;
    }

    void FindSettings::out_file_types(const std::unordered_set<FileType>& out_file_types) {
        m_out_file_types = out_file_types;
    }

    std::unordered_set<std::filesystem::path, PathHash> FindSettings::paths() const {
        return m_paths;
    }

    void FindSettings::paths(const std::unordered_set<std::filesystem::path, PathHash>& paths) {
        m_paths = paths;
    }

    bool FindSettings::print_dirs() const {
        return m_print_dirs;
    }

    void FindSettings::print_dirs(const bool print_dirs) {
        m_print_dirs = print_dirs;
    }

    bool FindSettings::print_files() const {
        return m_print_files;
    }

    void FindSettings::print_files(const bool print_files) {
        m_print_files = print_files;
    }

    bool FindSettings::print_usage() const {
        return m_print_usage;
    }

    void FindSettings::print_usage(const bool print_usage) {
        m_print_usage = print_usage;
    }

    bool FindSettings::print_version() const {
        return m_print_version;
    }

    void FindSettings::print_version(const bool print_version) {
        m_print_version = print_version;
    }

    bool FindSettings::recursive() const {
        return m_recursive;
    }

    void FindSettings::recursive(const bool recursive) {
        m_recursive = recursive;
    }

    SortBy FindSettings::sort_by() const {
        return m_sort_by;
    }

    void FindSettings::sort_by(const SortBy sort_by) {
        m_sort_by = sort_by;
    }

    bool FindSettings::sort_case_insensitive() const {
        return m_sort_case_insensitive;
    }

    void FindSettings::sort_case_insensitive(const bool sort_case_insensitive) {
        m_sort_case_insensitive = sort_case_insensitive;
    }

    bool FindSettings::sort_descending() const {
        return m_sort_descending;
    }

    void FindSettings::sort_descending(const bool sort_descending) {
        m_sort_descending = sort_descending;
    }

    bool FindSettings::verbose() const {
        return m_verbose;
    }

    void FindSettings::verbose(const bool verbose) {
        m_verbose = verbose;
    }

    void FindSettings::add_in_archive_extension(const std::string_view ext) {
        add_extensions(ext, m_in_archive_extensions);
    }

    void FindSettings::add_in_archive_file_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_in_archive_file_patterns);
    }

    void FindSettings::add_in_dir_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_in_dir_patterns);
    }

    void FindSettings::add_in_extension(const std::string_view ext) {
        add_extensions(ext, m_in_extensions);
    }

    void FindSettings::add_in_file_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_in_file_patterns);
    }

    void FindSettings::add_in_file_type(const FileType file_type) {
        m_in_file_types.emplace(file_type);
    }

    void FindSettings::add_out_archive_extension(const std::string_view ext) {
        add_extensions(ext, m_out_archive_extensions);
    }

    void FindSettings::add_out_archive_file_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_out_archive_file_patterns);
    }

    void FindSettings::add_out_dir_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_out_dir_patterns);
    }

    void FindSettings::add_out_extension(const std::string_view ext) {
        add_extensions(ext, m_out_extensions);
    }

    void FindSettings::add_out_file_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_out_file_patterns);
    }

    void FindSettings::add_out_file_type(const FileType file_type) {
        m_out_file_types.emplace(file_type);
    }

    void FindSettings::add_path(const std::filesystem::path& path) {
        m_paths.emplace(path);
    }

    void FindSettings::add_pattern(const std::string_view p, std::unordered_set<RegexPattern, RegexPatternHash>& ps) {
        const auto r = RegexPattern{std::string{p}};
        ps.insert(r);
    }

    void FindSettings::add_extensions(const std::string_view exts, std::unordered_set<std::string>& extensions) {
        const std::vector<std::string> xs = StringUtil::split_string(std::string{exts}, ",", true);
        for (const auto& x : xs) {
            if (!x.empty()) {
                extensions.emplace(x);
            }
        }
    }

    bool FindSettings::need_last_mod() const {
        return m_sort_by == SortBy::LASTMOD
               || m_max_last_mod != 0
               || m_min_last_mod != 0;
    }

    bool FindSettings::need_size() const {
        return m_sort_by == SortBy::FILESIZE
               || m_max_size > 0
               || m_min_size > 0;
    }

    bool FindSettings::need_stat() const {
        return need_size() || need_last_mod();
    }

    SortBy FindSettings::sort_by_from_name(const std::string_view name) {
        std::string lname{name};
        std::ranges::transform(lname.begin(), lname.end(), lname.begin(),
            [](const unsigned char c) { return std::tolower(c); });
        if (lname == SORT_BY_NAME_PATH || lname == SORT_BY_NAME_FILEPATH) {
            return SortBy::FILEPATH;
        }
        if (lname == SORT_BY_NAME_NAME || lname == SORT_BY_NAME_FILENAME) {
            return SortBy::FILENAME;
        }
        if (lname == SORT_BY_NAME_SIZE || lname == SORT_BY_NAME_FILESIZE) {
            return SortBy::FILESIZE;
        }
        if (lname == SORT_BY_NAME_TYPE || lname == SORT_BY_NAME_FILETYPE) {
            return SortBy::FILETYPE;
        }
        if (lname == SORT_BY_NAME_LASTMOD) {
            return SortBy::LASTMOD;
        }
        return SortBy::FILEPATH;
    }

    std::string FindSettings::sort_by_to_name(const SortBy sort_by) {
        switch (sort_by)
        {
            case SortBy::FILEPATH:
                return SORT_BY_NAME_FILEPATH;
            case SortBy::FILENAME:
                return SORT_BY_NAME_FILENAME;
            case SortBy::FILESIZE:
                return SORT_BY_NAME_FILESIZE;
            case SortBy::FILETYPE:
                return SORT_BY_NAME_FILETYPE;
            case SortBy::LASTMOD:
                return SORT_BY_NAME_LASTMOD;
            default:
                return SORT_BY_NAME_UNKNOWN;
        }
    }

    std::string FindSettings::file_types_to_string(const std::unordered_set<FileType>& types) {
        std::string ts_string = "[";
        int count = 0;
        for (auto const& t : types) {
            if (count > 0) {
                ts_string.append(", ");
            }
            ts_string.append(FileTypes::to_name(t));
            count++;
        }
        ts_string.append("]");
        return ts_string;
    }

    std::string FindSettings::patterns_to_string(const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
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

    std::string FindSettings::paths_to_string(const std::unordered_set<std::filesystem::path, PathHash>& paths) {
        std::string ss_string = "[";
        for (auto it = paths.cbegin(); it != paths.cend(); ++it) {
            ss_string.append("\"");
            ss_string.append(*it);
            ss_string.append("\"");
            if (std::next(it) != paths.end()) {
                ss_string.append(", ");
            }
        }
        ss_string.append("]");
        return ss_string;
    }

    std::string FindSettings::string() const {
        return std::string("FindSettings(")
                + "archives_only=" + StringUtil::bool_to_string(m_archives_only)
                + ", debug=" + StringUtil::bool_to_string(m_debug)
                + ", follow_symlinks=" + StringUtil::bool_to_string(m_follow_symlinks)
                + ", in_archive_extensions=" + StringUtil::unordered_string_set_to_string(m_in_archive_extensions)
                + ", in_archive_file_patterns=" + patterns_to_string(m_in_archive_file_patterns)
                + ", in_dir_patterns=" + patterns_to_string(m_in_dir_patterns)
                + ", in_extensions=" + StringUtil::unordered_string_set_to_string(m_in_extensions)
                + ", in_file_patterns=" + patterns_to_string(m_in_file_patterns)
                + ", in_file_types=" + file_types_to_string(m_in_file_types)
                + ", include_archives=" + StringUtil::bool_to_string(m_include_archives)
                + ", include_hidden=" + StringUtil::bool_to_string(m_include_hidden)
                + ", max_depth=" + std::to_string(m_max_depth)
                + ", max_last_mod=" + StringUtil::long_to_date_str(m_max_last_mod)
                + ", max_size=" + std::to_string(m_max_size)
                + ", min_depth=" + std::to_string(m_min_depth)
                + ", min_last_mod=" + StringUtil::long_to_date_str(m_min_last_mod)
                + ", min_size=" + std::to_string(m_min_size)
                + ", out_archive_extensions=" + StringUtil::unordered_string_set_to_string(m_out_archive_extensions)
                + ", out_archive_file_patterns=" + patterns_to_string(m_out_archive_file_patterns)
                + ", out_dir_patterns=" + patterns_to_string(m_out_dir_patterns)
                + ", out_extensions=" + StringUtil::unordered_string_set_to_string(m_out_extensions)
                + ", out_file_patterns=" + patterns_to_string(m_out_file_patterns)
                + ", out_file_types=" + file_types_to_string(m_out_file_types)
                + ", paths=" + paths_to_string(m_paths)
                + ", print_dirs=" + StringUtil::bool_to_string(m_print_dirs)
                + ", print_files=" + StringUtil::bool_to_string(m_print_files)
                + ", print_usage=" + StringUtil::bool_to_string(m_print_usage)
                + ", print_version=" + StringUtil::bool_to_string(m_print_version)
                + ", recursive=" + StringUtil::bool_to_string(m_recursive)
                + ", sort_by=" + sort_by_to_name(m_sort_by)
                + ", sort_case_insensitive=" + StringUtil::bool_to_string(m_sort_case_insensitive)
                + ", sort_descending=" + StringUtil::bool_to_string(m_sort_descending)
                + ", verbose=" + StringUtil::bool_to_string(m_verbose)
                + ")";
    }
}

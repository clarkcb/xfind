#ifndef CPPFIND_FINDSETTINGS_H
#define CPPFIND_FINDSETTINGS_H

#include <filesystem>
#include <string>
#include <unordered_set>

#include "FileTypes.h"
#include "RegexPattern.h"

#define SORT_BY_NAME_FILEPATH "filepath"
#define SORT_BY_NAME_PATH "path"
#define SORT_BY_NAME_FILENAME "filename"
#define SORT_BY_NAME_NAME "name"
#define SORT_BY_NAME_FILESIZE "filesize"
#define SORT_BY_NAME_SIZE "size"
#define SORT_BY_NAME_FILETYPE "filetype"
#define SORT_BY_NAME_TYPE "type"
#define SORT_BY_NAME_LASTMOD "lastmod"
#define SORT_BY_NAME_UNKNOWN "unknown"

namespace cppfind {
    enum class SortBy {FILEPATH, FILENAME, FILESIZE, FILETYPE, LASTMOD};

    struct PathHash {
        auto operator()(const std::filesystem::path& p) const noexcept {
            return std::filesystem::hash_value(p);
        }
    };

    class FindSettings {
    public:
        FindSettings();
        // property getters
        [[nodiscard]] bool archives_only() const;
        [[nodiscard]] bool debug() const;
        [[nodiscard]] bool include_archives() const;
        [[nodiscard]] bool include_hidden() const;
        [[nodiscard]] int max_depth() const;
        [[nodiscard]] long max_last_mod() const;
        [[nodiscard]] uint64_t max_size() const;
        [[nodiscard]] int min_depth() const;
        [[nodiscard]] long min_last_mod() const;
        [[nodiscard]] uint64_t min_size() const;
        [[nodiscard]] bool print_dirs() const;
        [[nodiscard]] bool print_files() const;
        [[nodiscard]] bool print_usage() const;
        [[nodiscard]] bool print_version() const;
        [[nodiscard]] bool recursive() const;
        [[nodiscard]] SortBy sort_by() const;
        [[nodiscard]] bool sort_case_insensitive() const;
        [[nodiscard]] bool sort_descending() const;
        [[nodiscard]] bool verbose() const;

        [[nodiscard]] std::unordered_set<std::string> in_archive_extensions() const;
        [[nodiscard]] std::unordered_set<RegexPattern, RegexPatternHash> in_archive_file_patterns() const;
        [[nodiscard]] std::unordered_set<RegexPattern, RegexPatternHash> in_dir_patterns() const;
        [[nodiscard]] std::unordered_set<std::string> in_extensions() const;
        [[nodiscard]] std::unordered_set<RegexPattern, RegexPatternHash> in_file_patterns() const;
        [[nodiscard]] std::unordered_set<FileType> in_file_types() const;
        [[nodiscard]] std::unordered_set<std::string> out_archive_extensions() const;
        [[nodiscard]] std::unordered_set<RegexPattern, RegexPatternHash> out_archive_file_patterns() const;
        [[nodiscard]] std::unordered_set<RegexPattern, RegexPatternHash> out_dir_patterns() const;
        [[nodiscard]] std::unordered_set<std::string> out_extensions() const;
        [[nodiscard]] std::unordered_set<RegexPattern, RegexPatternHash> out_file_patterns() const;
        [[nodiscard]] std::unordered_set<FileType> out_file_types() const;
        [[nodiscard]] std::unordered_set<std::filesystem::path, PathHash> paths() const;

        // property setters
        void archives_only(bool archives_only);
        void debug(bool debug);
        void in_archive_extensions(const std::unordered_set<std::string>& in_archive_extensions);
        void in_dir_patterns(const std::unordered_set<RegexPattern, RegexPatternHash>& in_dir_patterns);
        void in_extensions(const std::unordered_set<std::string>& in_extensions);
        void in_file_patterns(const std::unordered_set<RegexPattern, RegexPatternHash>& in_file_patterns);
        void in_file_types(const std::unordered_set<FileType>& in_file_types);
        void include_archives(bool include_archives);
        void include_hidden(bool include_hidden);
        void max_depth(int max_depth);
        void max_last_mod(long max_last_mod);
        void max_size(uint64_t max_size);
        void min_depth(int min_depth);
        void min_last_mod(long min_last_mod);
        void min_size(uint64_t min_size);
        void out_archive_extensions(const std::unordered_set<std::string>& out_archive_extensions);
        void out_dir_patterns(const std::unordered_set<RegexPattern, RegexPatternHash>& out_dir_patterns);
        void out_extensions(const std::unordered_set<std::string>& out_extensions);
        void out_file_patterns(const std::unordered_set<RegexPattern, RegexPatternHash>& out_file_patterns);
        void out_file_types(const std::unordered_set<FileType>& out_file_types);
        void paths(const std::unordered_set<std::filesystem::path, PathHash>& paths);
        void print_dirs(bool print_dirs);
        void print_files(bool print_files);
        void print_usage(bool print_usage);
        void print_version(bool print_version);
        void recursive(bool recursive);
        void sort_by(SortBy sort_by);
        void sort_case_insensitive(bool sort_case_insensitive);
        void sort_descending(bool sort_descending);
        void verbose(bool verbose);

        // add elements methods
        void add_in_archive_extension(std::string_view ext);
        void add_in_archive_file_pattern(std::string_view pattern);
        void add_in_dir_pattern(std::string_view pattern);
        void add_in_extension(std::string_view ext);
        void add_in_file_pattern(std::string_view pattern);
        void add_in_file_type(FileType file_type);
        void add_out_archive_extension(std::string_view ext);
        void add_out_archive_file_pattern(std::string_view pattern);
        void add_out_dir_pattern(std::string_view pattern);
        void add_out_extension(std::string_view ext);
        void add_out_file_pattern(std::string_view pattern);
        void add_out_file_type(FileType file_type);
        void add_path(const std::filesystem::path& path);

        // need elements methods
        [[nodiscard]] bool need_size() const;
        [[nodiscard]] bool need_last_mod() const;
        [[nodiscard]] bool need_stat() const;

        // utility methods
        static void add_pattern(std::string_view p, std::unordered_set<RegexPattern, RegexPatternHash>& ps);
        static void add_extensions(std::string_view exts, std::unordered_set<std::string>& extensions);
        static std::string file_types_to_string(const std::unordered_set<FileType>& types);
        static std::string patterns_to_string(const std::unordered_set<RegexPattern, RegexPatternHash>& patterns);
        static std::string paths_to_string(const std::unordered_set<std::filesystem::path, PathHash>& paths);
        static SortBy sort_by_from_name(std::string_view name);
        static std::string sort_by_to_name(SortBy sort_by);

        // convert to string
        [[nodiscard]] std::string string() const;

    protected:
        bool m_archives_only;
        bool m_debug;

        // add sets of strings that hold extensions and patterns to match on
        std::unordered_set<std::string> m_in_archive_extensions;
        std::unordered_set<RegexPattern, RegexPatternHash> m_in_archive_file_patterns;
        std::unordered_set<RegexPattern, RegexPatternHash> m_in_dir_patterns;
        std::unordered_set<std::string> m_in_extensions;
        std::unordered_set<RegexPattern, RegexPatternHash> m_in_file_patterns;
        std::unordered_set<FileType> m_in_file_types;

        bool m_include_archives;
        bool m_include_hidden;

        int m_max_depth;
        long m_max_last_mod;
        uint64_t m_max_size;
        int m_min_depth;
        long m_min_last_mod;
        uint64_t m_min_size;

        // add corresponding sets for out extensions and patterns
        std::unordered_set<std::string> m_out_archive_extensions;
        std::unordered_set<RegexPattern, RegexPatternHash> m_out_archive_file_patterns;
        std::unordered_set<RegexPattern, RegexPatternHash> m_out_dir_patterns;
        std::unordered_set<std::string> m_out_extensions;
        std::unordered_set<RegexPattern, RegexPatternHash> m_out_file_patterns;
        std::unordered_set<FileType> m_out_file_types;

        bool m_print_dirs;
        bool m_print_files;
        bool m_print_usage;
        bool m_print_version;
        bool m_recursive;

        std::unordered_set<std::filesystem::path, PathHash> m_paths;

        SortBy m_sort_by = SortBy::FILEPATH;
        bool m_sort_case_insensitive;
        bool m_sort_descending;
        bool m_verbose;
    };
}

#endif // CPPFIND_FINDSETTINGS_H

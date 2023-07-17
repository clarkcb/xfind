#ifndef CPPFIND_FINDSETTINGS_H
#define CPPFIND_FINDSETTINGS_H

#include <cstdlib>
#include "FileTypes.h"
#include "RegexPattern.h"

namespace cppfind {
    enum class SortBy {FILEPATH, FILENAME, FILESIZE, FILETYPE, LASTMOD};

    class FindSettings {
    protected:
        bool m_archives_only = false;
        bool m_debug = false;
        bool m_exclude_hidden = true;

        std::vector<std::string> m_in_archive_extensions;
        std::vector<RegexPattern*> m_in_archive_file_patterns;
        std::vector<RegexPattern*> m_in_dir_patterns;
        std::vector<std::string> m_in_extensions;
        std::vector<RegexPattern*> m_in_file_patterns;
        std::vector<FileType> m_in_file_types;

        bool m_include_archives = false;
        bool m_list_dirs = false;
        bool m_list_files = false;

        long m_max_last_mod = 0L;
        long m_max_size = 0L;
        long m_min_last_mod = 0L;
        long m_min_size = 0L;

        std::vector<std::string> m_out_archive_extensions;
        std::vector<RegexPattern*> m_out_archive_file_patterns;
        std::vector<RegexPattern*> m_out_dir_patterns;
        std::vector<std::string> m_out_extensions;
        std::vector<RegexPattern*> m_out_file_patterns;
        std::vector<FileType> m_out_file_types;

        bool m_print_usage = false;
        bool m_print_version = false;
        bool m_recursive = true;

        std::vector<std::string> m_paths;

        SortBy m_sort_by = SortBy::FILEPATH;
        bool m_sort_case_insensitive = false;
        bool m_sort_descending = false;
        bool m_verbose = false;

        static std::string bool_to_string(bool b);
        static std::string string_vector_to_string(std::vector<std::string>* s);
        static std::string find_patterns_to_string(std::vector<RegexPattern*>* ps);
        static std::string file_types_to_string(std::vector<FileType>* ts);

        static void add_pattern(const std::string& p, std::vector<RegexPattern*>* ps);
        static void add_extensions(const std::string& exts, std::vector<std::string>* extensions);

    public:
        FindSettings();
        void add_in_archive_extension(const std::string& ext);
        void add_in_archive_file_pattern(const std::string& pattern);
        void add_in_dir_pattern(const std::string& pattern);
        void add_in_extension(const std::string& ext);
        void add_in_file_pattern(const std::string& pattern);
        void add_in_file_type(FileType file_type);
        void add_out_archive_extension(const std::string& ext);
        void add_out_archive_file_pattern(const std::string& pattern);
        void add_out_dir_pattern(const std::string& pattern);
        void add_out_extension(const std::string& ext);
        void add_out_file_pattern(const std::string& pattern);
        void add_out_file_type(FileType file_type);
        void add_path(const std::string& path);

        [[nodiscard]] bool archives_only() const;
        [[nodiscard]] bool debug() const;
        [[nodiscard]] bool exclude_hidden() const;
        [[nodiscard]] bool include_archives() const;
        [[nodiscard]] bool list_dirs() const;
        [[nodiscard]] bool list_files() const;
        [[nodiscard]] long max_last_mod() const;
        [[nodiscard]] long max_size() const;
        [[nodiscard]] long min_last_mod() const;
        [[nodiscard]] long min_size() const;
        [[nodiscard]] bool print_usage() const;
        [[nodiscard]] bool print_version() const;
        [[nodiscard]] bool recursive() const;
        [[nodiscard]] bool sort_case_insensitive() const;
        [[nodiscard]] bool sort_descending() const;
        [[nodiscard]] bool verbose() const;

        std::vector<std::string>* in_archive_extensions();
        std::vector<RegexPattern*>* in_archive_file_patterns();
        std::vector<RegexPattern*>* in_dir_patterns();
        std::vector<std::string>* in_extensions();
        std::vector<RegexPattern*>* in_file_patterns();
        std::vector<FileType>* in_file_types();

        std::vector<std::string>* out_archive_extensions();
        std::vector<RegexPattern*>* out_archive_file_patterns();
        std::vector<RegexPattern*>* out_dir_patterns();
        std::vector<std::string>* out_extensions();
        std::vector<RegexPattern*>* out_file_patterns();
        std::vector<FileType>* out_file_types();

        bool need_stat();
        std::vector<std::string>* paths();

        SortBy sort_by();

        void archives_only(bool b);
        void debug(bool b);
        void exclude_hidden(bool b);
        void include_archives(bool b);
        void list_dirs(bool b);
        void list_files(bool b);
        void max_last_mod(long max_last_mod);
        void max_size(long max_size);
        void min_last_mod(long min_last_mod);
        void min_size(long min_size);
        void print_usage(bool b);
        void print_version(bool b);
        void recursive(bool b);
        void sort_by(SortBy sort_by);
        void set_sort_by(const std::string& name);
        void sort_case_insensitive(bool b);
        void sort_descending(bool b);
        void verbose(bool b);
        std::string string();

        static SortBy sort_by_from_name(const std::string& name);
        static std::string sort_by_to_name(SortBy sort_by);
    };
}

#endif //CPPFIND_FINDSETTINGS_H

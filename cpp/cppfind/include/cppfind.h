#ifndef CPPFIND_H
#define CPPFIND_H

/*
 * cppfind.h - the public header file for using cppfind as a library dependency
 *
 * This file was created by:
 * 1) concatenating all other header files into a single file
 * 2) removing private sections from classes
 * 3) consolidating all definitions under a single namespace
 * 4) removing redundant / unneeded imports
 *
 * The concatenation of the header files was done by temporarily adding the following
 * to the CMakeLists.txt file (from https://stackoverflow.com/a/15283110/1002072):
 *
 * function(cat IN_FILE OUT_FILE)
 *         message("cat ${IN_FILE} ${OUT_FILE}")
 *         file(READ ${IN_FILE} CONTENTS)
 *         file(APPEND ${OUT_FILE} "${CONTENTS}")
 * endfunction()
 *
 * set(HEADER_FILES
 *         include/common.h
 *         include/config.h
 *         include/FileTypes.h
 *         include/FileUtil.h
 *         include/StringUtil.h
 *         include/FindException.h
 *         include/FindSettings.h
 *         include/FileResult.h
 *         include/FindOption.h
 *         include/FindOptions.h
 *         include/FindPattern.h
 *         include/Finder.h)
 *
 * # Prepare a temporary file to "cat" to:
 * file(WRITE cppfind.h.in "")
 *
 * # Call the "cat" function for each input file
 * foreach(HEADER_FILE ${HEADER_FILES})
 *         cat(${HEADER_FILE} cppfind.h.in)
 * endforeach()
 *
 * # Copy the temporary file to the final location
 * configure_file(cppfind.h.in cppfind.h COPYONLY)
 *
 */

#include <cstdlib>
#include <regex>
#include <set>
#include <string>
#include <sys/stat.h>
#include <unordered_map>
#include <vector>

#include "rapidjson/document.h"

using namespace rapidjson;


namespace cppfind {
    // common.h
    long datestr_to_long(const std::string& datestr);
    std::string long_to_datestr(long time);
    void log(const std::string& name);
    void log_error(const std::string& name);

    // config.h
    std::string xfindpath();

    // FileTypes.h
    enum class FileType {UNKNOWN, ARCHIVE, BINARY, CODE, TEXT, XML};

    class FileTypes {
    public:
        FileTypes();
        static FileType from_name(const std::string& name);
        static std::string to_name(FileType file_type);
        FileType get_file_type(const std::string& file_path);
        bool is_archive_file(const std::string& file_path);
        bool is_binary_file(const std::string& file_path);
        bool is_code_file(const std::string& file_path);
        bool is_text_file(const std::string& file_path);
        bool is_unknown_file(const std::string& file_path);
        bool is_xml_file(const std::string& file_path);
    };

    // FileUtil.h
    class FileUtil {
    public:
        static std::string expand_path(const std::string& file_path);
        static bool file_exists(const std::string& file_path);
        static uint64_t file_size(const std::string& file_path);
        static std::string get_contents(const std::string& file_path);
        static std::string get_contents(const std::ifstream& fin);
        static std::string get_extension(const std::string& name);
        static std::string get_file_name(const std::string& file_path);
        static bool is_directory(const std::string& name);
        static bool is_regular_file(const std::string& name);
        static bool is_dot_dir(const std::string& name);
        static bool is_hidden(const std::string& name);
        static std::string join_path(const std::string& path1, const std::string& path2);
        static std::vector<std::string> split_path(const std::string& file_path);
    };

    // StringUtil.h
    class StringUtil {
    public:
        static std::vector<std::string> split_string(const std::string& s, const std::string& delims);
        static void ltrim(std::string& s);
        static std::string ltrim_copy(std::string s);
        static void rtrim(std::string& s);
        static std::string rtrim_copy(std::string s);
        static void trim(std::string& s);
        static std::string trim_copy(std::string s);
    };

    // FindException.h
    class FindException : public std::exception {
    public:
        explicit FindException(const std::string& message);
        [[nodiscard]] std::string msg() const noexcept;
        [[nodiscard]] const char *what() const noexcept override;
    };

    // RegexPattern.h
    class RegexPattern {
    private:
        std::string m_pattern;
        std::regex m_regex;

    public:
        explicit RegexPattern(const std::string& pattern);
        [[nodiscard]] std::string pattern() const;
        [[nodiscard]] std::regex r() const;
    };

    // FindSettings.h
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

    // FileResult.h
    class FileResult {
    public:
        FileResult(std::string& path, std::string& file_name, FileType file_type, uint64_t file_size, long mod_time);
        FileResult(const std::vector<std::string>& containers, std::string& path, std::string& file_name,
                   FileType file_type, uint64_t file_size, long mod_time);
        FileResult(const std::vector<std::string>& containers, const std::string& path, const std::string& file_name,
                   FileType file_type, uint64_t file_size, long mod_time);
        [[nodiscard]] std::string path() const;
        [[nodiscard]] std::string file_name() const;
        [[nodiscard]] FileType file_type() const;
        [[nodiscard]] uint64_t file_size() const;
        [[nodiscard]] long mod_time() const;
        [[nodiscard]] const std::string string() const;
    };

    // FindOption.h
    class FindOption {
    public:
        FindOption(const std::string* shortarg, const std::string& longarg, const std::string& description);
        [[nodiscard]] const std::string* shortarg() const;
        [[nodiscard]] std::string longarg() const;
        [[nodiscard]] std::string description() const;
        [[nodiscard]] std::string sortarg() const;
    };

    // FindOptions.h
    class FindOptions {
    public:
        FindOptions();
        FindSettings* settings_from_args(int &argc, char **argv);
        void settings_from_json(std::string& json, FindSettings* settings);
        void usage();
        std::string get_usage_string();
    };

    // Finder.h
    class Finder {
    public:
        explicit Finder(FindSettings* settings);
        std::optional<FileResult*> filter_to_file_result(const std::string& file_path);
        bool is_matching_archive_file(const std::string& file_name);
        bool is_matching_dir(const std::string& file_path);
        bool is_matching_file(const std::string& file_name, FileType file_type, const struct stat*);
        std::vector<FileResult*> find();
    };

}

#endif //CPPFIND_H
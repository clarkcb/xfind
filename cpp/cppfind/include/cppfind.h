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
    void log(const std::string& name);
    void log_error(const std::string& name);

    // config.h
    std::string xfindpath();

    // FileTypes.h
    enum class FileType {UNKNOWN, ARCHIVE, BINARY, CODE, TEXT, XML};

    class FileTypes {
    public:
        FileTypes();
        ~FileTypes();
        static FileType from_name(const std::string& name);
        static std::string to_name(const FileType& file_type);
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
//        static std::string expand_path(const std::string& file_path);
        static bool file_exists(const std::string& file_path);
        static uint64_t file_size(const std::string& file_path);
//        static std::string get_contents(const std::string& file_path);
        static std::string get_contents(const std::ifstream& fin);
        static std::string get_extension(const std::string& name);
        static std::string get_file_name(const std::string& file_path);
        static bool is_directory(const std::string& name);
        static bool is_regular_file(const std::string& name);
        static bool is_dot_dir(const std::string& name);
        static bool is_hidden(const std::string& name);
        static std::string join_path(const std::string& path1, const std::string& path2);
        static std::pair<std::string, std::string> split_path(const std::string& file_path);
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
        static bool char_in_string(const char c, const std::string& s);
        static bool string_in_set(const std::string& s, const std::set<std::string>& set);
        static bool string_in_vector(const std::string& s, const std::vector<std::string>& set);
        static std::vector<std::string> filter_string_vector(const std::vector<std::string>& strings,
                                                             const std::function<bool(const std::string&)>& predicate);
        static std::string bool_to_string(bool b);
        static std::string string_set_to_string(std::set<std::string>& set);
        static long datestr_to_long(const std::string& datestr);
        static std::string long_to_datestr(const long time);
    };

    // FindException.h
    class FindException : public std::exception {
    public:
        explicit FindException(const std::string& message);
        [[nodiscard]] std::string message() const noexcept;
        [[nodiscard]] const char *what() const noexcept override;
    };

    // RegexPattern.h
    class RegexPattern {
    public:
        explicit RegexPattern(const std::string& pattern);
        RegexPattern(const std::string& pattern, bool ignore_case, bool multi_line, bool dot_all);
        [[nodiscard]] std::string pattern() const;
        [[nodiscard]] bool ignore_case() const;
        [[nodiscard]] bool multi_line() const;
        [[nodiscard]] bool dot_all() const;
        [[nodiscard]] std::regex regex() const;
        std::string string();
    };

    struct RegexPatternCmp
    {
        bool operator()(const RegexPattern& lhs, const RegexPattern& rhs) const
        {
            return lhs.pattern() < rhs.pattern();
        }
    };

    // FindSettings.h
    enum class SortBy {FILEPATH, FILENAME, FILESIZE, FILETYPE, LASTMOD};

    class FindSettings {
    public:
        // property getters
        [[nodiscard]] bool archives_only() const;
        [[nodiscard]] bool debug() const;
        [[nodiscard]] bool include_archives() const;
        [[nodiscard]] bool include_hidden() const;
        [[nodiscard]] bool list_dirs() const;
        [[nodiscard]] bool list_files() const;
        [[nodiscard]] bool print_usage() const;
        [[nodiscard]] bool print_version() const;
        [[nodiscard]] bool recursive() const;
        [[nodiscard]] bool sort_case_insensitive() const;
        [[nodiscard]] bool sort_descending() const;
        [[nodiscard]] bool verbose() const;
        [[nodiscard]] int max_depth() const;
        [[nodiscard]] int min_depth() const;
        [[nodiscard]] long max_last_mod() const;
        [[nodiscard]] long max_size() const;
        [[nodiscard]] long min_last_mod() const;
        [[nodiscard]] long min_size() const;
        [[nodiscard]] SortBy sort_by() const;

        [[nodiscard]] std::set<std::string> in_archive_extensions() const;
        [[nodiscard]] std::set<RegexPattern, RegexPatternCmp> in_archive_file_patterns() const;
        [[nodiscard]] std::set<RegexPattern, RegexPatternCmp> in_dir_patterns() const;
        [[nodiscard]] std::set<std::string> in_extensions() const;
        [[nodiscard]] std::set<RegexPattern, RegexPatternCmp> in_file_patterns() const;
        [[nodiscard]] std::set<FileType> in_file_types() const;
        [[nodiscard]] std::set<std::string> out_archive_extensions() const;
        [[nodiscard]] std::set<RegexPattern, RegexPatternCmp> out_archive_file_patterns() const;
        [[nodiscard]] std::set<RegexPattern, RegexPatternCmp> out_dir_patterns() const;
        [[nodiscard]] std::set<std::string> out_extensions() const;
        [[nodiscard]] std::set<RegexPattern, RegexPatternCmp> out_file_patterns() const;
        [[nodiscard]] std::set<FileType> out_file_types() const;
        [[nodiscard]] std::set<std::string> paths() const;

        // property setters
        void archives_only(bool archives_only);
        void debug(bool debug);
        void in_archive_extensions(const std::set<std::string>& in_archive_extensions);
        void in_dir_patterns(const std::set<RegexPattern, RegexPatternCmp>& in_dir_patterns);
        void in_extensions(const std::set<std::string>& in_extensions);
        void in_file_patterns(const std::set<RegexPattern, RegexPatternCmp>& in_file_patterns);
        void in_file_types(const std::set<FileType>& in_file_types);
        void include_archives(bool include_archives);
        void include_hidden(bool exclude_hidden);
        void list_dirs(bool list_dirs);
        void list_files(bool list_files);
        void max_depth(int max_depth);
        void max_last_mod(long max_last_mod);
        void max_size(long max_size);
        void min_depth(int min_depth);
        void min_last_mod(long min_last_mod);
        void min_size(long min_size);
        void out_archive_extensions(const std::set<std::string>& out_archive_extensions);
        void out_dir_patterns(const std::set<RegexPattern, RegexPatternCmp>& out_dir_patterns);
        void out_extensions(const std::set<std::string>& out_extensions);
        void out_file_patterns(const std::set<RegexPattern, RegexPatternCmp>& out_file_patterns);
        void out_file_types(const std::set<FileType>& out_file_types);
        void paths(const std::set<std::string>& paths);
        void print_usage(bool print_usage);
        void print_version(bool print_version);
        void recursive(bool recursive);
        void sort_by(SortBy sort_by);
        void sort_case_insensitive(bool sort_case_insensitive);
        void sort_descending(bool sort_descending);
        void verbose(bool verbose);

        // add elements methods
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

        // utility methods
        static void add_pattern(const std::string& p, std::set<RegexPattern, RegexPatternCmp>& ps);
        static void add_extensions(const std::string& exts, std::set<std::string>& extensions);
        static std::string find_patterns_to_string(std::set<RegexPattern, RegexPatternCmp>& patterns);
        static std::string file_types_to_string(std::set<FileType>& types);
        static SortBy sort_by_from_name(const std::string& name);
        static std::string sort_by_to_name(SortBy sort_by);

        // convert to string
        std::string string();
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
        FindOption(const std::string& short_arg, const std::string& long_arg, const std::string& description);
        [[nodiscard]] std::string short_arg() const;
        [[nodiscard]] std::string long_arg() const;
        [[nodiscard]] std::string description() const;
        [[nodiscard]] std::string sort_arg() const;
    };

    // FindOptions.h
    class FindOptions {
    public:
        FindOptions();
        FindSettings settings_from_args(int &argc, char **argv);
        void settings_from_file(std::string& file_path, FindSettings& settings);
        void settings_from_json(std::string& json, FindSettings& settings);
        void usage();
        std::string get_usage_string();
    };

    // Finder.h
    class Finder {
    public:
        explicit Finder(const FindSettings& settings);
        // bool filter_file(const std::string& file_path);
        std::optional<FileResult> filter_to_file_result(const std::string& file_path);
        bool is_matching_archive_file(const std::string& file_name);
        bool is_matching_dir(const std::string& file_path);
        bool is_matching_file(const std::string& file_name, const FileType file_type, const struct stat*);
        bool is_matching_file_type(const FileType file_type);
        bool is_matching_file_result(const FileResult* file_result);
        std::vector<FileResult> find();
    };

}

#endif // CPPFIND_H

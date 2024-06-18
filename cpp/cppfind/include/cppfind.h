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

#include <filesystem>
#include <regex>
#include <string>
#include <unordered_set>
#include <vector>

#include "rapidjson/document.h"

namespace cppfind {

    // FileTypes.h
    enum class FileType {UNKNOWN, ARCHIVE, AUDIO, BINARY, CODE, FONT, IMAGE, TEXT, VIDEO, XML};

    class FileTypes {
    public:
        FileTypes();
        ~FileTypes() = default;
        static FileType from_name(std::string_view name);
        static std::string to_name(const FileType& file_type);
        [[nodiscard]] FileType get_path_type(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_archive_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_audio_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_binary_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_code_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_font_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_image_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_text_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_unknown_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_video_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_xml_path(const std::filesystem::path& file_path) const;
    };

    // FileUtil.h
    class FileUtil {
    public:
        static std::filesystem::path expand_tilde(const std::filesystem::path& path);
        static bool path_exists(const std::filesystem::path& path);
        static std::string get_path_extension(const std::filesystem::path& file_path);
        static bool is_dot_dir(std::string_view file_name);
        static bool is_hidden(std::string_view file_name);
        static bool is_hidden_path(const std::filesystem::path& file_path);
    };

    // FindConfig.h
    std::string xfindpath();

    // FindException.h
    class FindException : public std::exception {
    public:
        explicit FindException(std::string_view message);
        [[nodiscard]] std::string message() const noexcept;
        [[nodiscard]] const char *what() const noexcept override;
    };

    // FindOption.h
    class FindOption {
    public:
        FindOption(std::string_view short_arg, std::string_view long_arg, std::string_view description);
        FindOption() = delete;
        [[nodiscard]] std::string short_arg() const;
        [[nodiscard]] std::string long_arg() const;
        [[nodiscard]] std::string description() const;
        [[nodiscard]] std::string sort_arg() const;
    };

    // RegexPattern.h
    class RegexPattern {
    public:
        explicit RegexPattern(std::string_view pattern);
        RegexPattern(std::string_view pattern, bool ignore_case, bool multi_line, bool dot_all);
        RegexPattern() = delete;
        [[nodiscard]] std::string pattern() const;
        [[nodiscard]] bool ignore_case() const;
        [[nodiscard]] bool multi_line() const;
        [[nodiscard]] bool dot_all() const;
        [[nodiscard]] std::regex regex() const;
        [[nodiscard]] std::string string() const;
        bool operator==(const RegexPattern& other) const;
    };

    struct RegexPatternHash {
        std::size_t operator()(const RegexPattern& r) const noexcept {
            constexpr std::hash<std::string> string_hash;
            return string_hash(r.pattern());
        }
    };

    // StringUtil.h
    class StringUtil {
    public:
        static std::vector<std::string> split_string(std::string_view s, std::string_view delims);
        static std::vector<std::string> split_string(std::string_view s, std::string_view delims, bool exclude_empty);
        static void ltrim(std::string& s);
        static std::string ltrim_copy(std::string s);
        static void rtrim(std::string& s);
        static std::string rtrim_copy(std::string s);
        static void trim(std::string& s);
        static std::string trim_copy(std::string s);

        static bool char_in_string(char c, std::string_view s);
        static bool string_in_unordered_set(std::string_view s, const std::unordered_set<std::string>& set);
        static bool string_in_vector(std::string_view s, const std::vector<std::string>& vec);

        static std::vector<std::string> filter_string_vector(const std::vector<std::string>& vec,
                                                             const std::function<bool(const std::string&)>& predicate);
        static std::string bool_to_string(bool b);
        static std::string unordered_string_set_to_string(const std::unordered_set<std::string>& set);
        static long date_str_to_long(std::string_view date_str);
        static std::string long_to_date_str(long time);
    };

    // common.h
    void log(std::string_view msg);
    void log_error(std::string_view msg);

    // FindSettings.h
    enum class SortBy {FILEPATH, FILENAME, FILESIZE, FILETYPE, LASTMOD};

    struct PathHash {
        auto operator()(const std::filesystem::path& p) const noexcept {
            return std::filesystem::hash_value(p);
        }
    };

    class FindSettings {
    public:
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
    };

    // FileResult.h
    class FileResult {
    public:
        explicit FileResult(const std::filesystem::path& file_path, FileType file_type, uint64_t file_size,
                   long last_mod);
        explicit FileResult(const std::vector<std::filesystem::path>& containers, const std::filesystem::path& file_path,
                   FileType file_type, uint64_t file_size, long last_mod);
        [[nodiscard]] std::vector<std::filesystem::path> containers() const;
        [[nodiscard]] std::filesystem::path file_path() const;
        [[nodiscard]] FileType file_type() const;
        [[nodiscard]] uint64_t file_size() const;
        [[nodiscard]] long last_mod() const;
        [[nodiscard]] std::string string() const;
    };

    // FindOptions.h
    class FindOptions {
    public:
        FindOptions();
        FindSettings settings_from_args(int &argc, char **argv);
        void settings_from_file(const std::filesystem::path& file_path, FindSettings& settings);
        void settings_from_json(std::string_view json_str, FindSettings& settings);
        void usage();
        std::string get_usage_string();
    };

    // Finder.h
    class Finder {
    public:
        explicit Finder(const FindSettings& settings);
        explicit Finder(const std::unique_ptr<FindSettings>& settings_ptr);
        Finder(Finder& other) = delete;
        Finder(Finder&& other) = delete;
        std::optional<FileResult> filter_to_file_result(std::filesystem::path&& file_path) const;
        [[nodiscard]] bool is_matching_archive_file_result(const FileResult& file_result) const;
        [[nodiscard]] bool is_matching_dir_path(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool has_matching_archive_extension(const FileResult& file_result) const;
        [[nodiscard]] bool has_matching_extension(const FileResult& file_result) const;
        [[nodiscard]] bool has_matching_archive_file_name(const FileResult& file_result) const;
        [[nodiscard]] bool has_matching_file_name(const FileResult& file_result) const;
        [[nodiscard]] bool has_matching_file_type(const FileResult& file_result) const;
        [[nodiscard]] bool has_matching_file_size(const FileResult& file_result) const;
        [[nodiscard]] bool has_matching_last_mod(const FileResult& file_result) const;
        [[nodiscard]] bool is_matching_file_result(const FileResult& file_result) const;
        std::vector<FileResult> find();
    };
}

#endif // CPPFIND_H

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

#include <any>
#include <filesystem>
#include <regex>
#include <string>
#include <unordered_set>
#include <vector>

#include "rapidjson/document.h"

namespace cppfind {

    // FileTypes.h
#define FILE_TYPE_NAME_ARCHIVE "archive"
#define FILE_TYPE_NAME_AUDIO "audio"
#define FILE_TYPE_NAME_BINARY "binary"
#define FILE_TYPE_NAME_CODE "code"
#define FILE_TYPE_NAME_FONT "font"
#define FILE_TYPE_NAME_IMAGE "image"
#define FILE_TYPE_NAME_TEXT "text"
#define FILE_TYPE_NAME_VIDEO "video"
#define FILE_TYPE_NAME_XML "xml"
#define FILE_TYPE_NAME_NOSEARCH "nosearch"
#define FILE_TYPE_NAME_UNKNOWN "unknown"

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
        static std::filesystem::path expand_path(const std::filesystem::path& path);
        static bool path_exists(const std::filesystem::path& path);
        static std::string get_path_extension(const std::filesystem::path& file_path);
        static bool is_dot_dir(std::string_view file_name);
        static bool is_hidden(std::string_view file_name);
        static bool is_hidden_path(const std::filesystem::path& file_path);
    };

    // FindConfig.h
    std::string xfindpath();

    // FindException.h
#define INVALID_RANGE_MINDEPTH_MAXDEPTH "Invalid range for mindepth and maxdepth"
#define INVALID_RANGE_MINSIZE_MAXSIZE "Invalid range for minsize and maxsize"
#define INVALID_RANGE_MINLASTMOD_MAXLASTMOD "Invalid range for minlastmod and maxlastmod"
#define STARTPATH_NOT_DEFINED "Startpath not defined"
#define STARTPATH_NOT_FOUND "Startpath not found"
#define STARTPATH_NOT_READABLE "Startpath not readable"

    class FindException : public std::exception {
    public:
        explicit FindException(std::string_view message);
        [[nodiscard]] std::string message() const noexcept;
        [[nodiscard]] const char *what() const noexcept override;
    };

    // Option.h
    class Option {
    public:
        [[nodiscard]] virtual std::string short_arg() const = 0;
        [[nodiscard]] virtual std::string long_arg() const = 0;
        [[nodiscard]] virtual std::string description() const = 0;
        [[nodiscard]] virtual int arg_type() const = 0;
        [[nodiscard]] virtual std::string sort_arg() const = 0;
        virtual ~Option() = default;
    };

    // FindOption.h
    class FindOption : public Option {
    public:
        FindOption(std::string_view short_arg, std::string_view long_arg, std::string_view description, int arg_type);
        FindOption() = delete;
        [[nodiscard]] std::string short_arg() const override;
        [[nodiscard]] std::string long_arg() const override;
        [[nodiscard]] std::string description() const override;
        [[nodiscard]] int arg_type() const override;
        [[nodiscard]] std::string sort_arg() const override;
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

    // consolecolor.h
#define CONSOLE_RESET   "\033[0m"
#define CONSOLE_BLACK   "\033[0;30m"
#define CONSOLE_RED     "\033[0;31m"
#define CONSOLE_GREEN   "\033[0;32m"
#define CONSOLE_YELLOW  "\033[0;33m"
#define CONSOLE_BLUE    "\033[0;34m"
#define CONSOLE_MAGENTA "\033[0;35m"
#define CONSOLE_CYAN    "\033[0;36m"
#define CONSOLE_WHITE   "\033[0;37m"

#define BOLD_BLACK      "\033[1;30m"
#define BOLD_RED        "\033[1;31m"
#define BOLD_GREEN      "\033[1;32m"
#define BOLD_YELLOW     "\033[1;33m"
#define BOLD_BLUE       "\033[1;34m"
#define BOLD_MAGENTA    "\033[1;35m"
#define BOLD_CYAN       "\033[1;36m"
#define BOLD_WHITE      "\033[1;37m"

    enum class Color {BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE};

    Color color_from_name(std::string_view name);
    std::string color_to_name(Color color);
    std::string color_to_console_color(Color color);

    // common.h
    void log_msg(std::string_view msg);
    void log_error(std::string_view msg);
    void log_error_color(std::string_view msg, bool colorize);

    // ArgToken.h
    class ArgToken {
    public:
        ArgToken(std::string_view name, uint8_t token_type, const std::any &value);
        ArgToken() = delete;
        [[nodiscard]] std::string name() const;
        [[nodiscard]] uint8_t token_type() const;
        [[nodiscard]] std::any value() const;
    };

    // ArgTokenizer.h
#define ARG_TOKEN_TYPE_UNKNOWN 0
#define ARG_TOKEN_TYPE_BOOL    1
#define ARG_TOKEN_TYPE_STR     2
#define ARG_TOKEN_TYPE_INT     3
#define ARG_TOKEN_TYPE_LONG    4

    class ArgTokenizer {
    public:
        ArgTokenizer() = delete;
        explicit ArgTokenizer(const std::vector<std::unique_ptr<Option>>& options);
        [[nodiscard]] std::vector<ArgToken> tokenize_args(int argc, char **argv) const;
        [[nodiscard]] std::vector<ArgToken> tokenize_json(std::string_view json) const;
        [[nodiscard]] std::vector<ArgToken> tokenize_file(const std::filesystem::path& file_path) const;
    };

    // FindSettings.h
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

    enum class SortBy {FILEPATH, FILENAME, FILESIZE, FILETYPE, LASTMOD};

    struct PathHash {
        auto operator()(const std::filesystem::path& p) const noexcept {
            return std::filesystem::hash_value(p);
        }
    };

    class FindSettings {
    public:
        FindSettings();
        FindSettings(const FindSettings &) = default;
        FindSettings(FindSettings &&) = default;
        ~FindSettings() = default;
        FindSettings &operator=(const FindSettings &) = default;
        FindSettings &operator=(FindSettings &&) = default;

        // property getters
        [[nodiscard]] bool archives_only() const;
        [[nodiscard]] bool colorize() const;
        [[nodiscard]] bool debug() const;
        [[nodiscard]] Color dir_color() const;
        [[nodiscard]] Color ext_color() const;
        [[nodiscard]] Color file_color() const;
        [[nodiscard]] bool follow_symlinks() const;
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

        [[nodiscard]] const std::unordered_set<std::string>& in_archive_extensions() const;
        [[nodiscard]] const std::unordered_set<RegexPattern, RegexPatternHash>& in_archive_file_patterns() const;
        [[nodiscard]] const std::unordered_set<RegexPattern, RegexPatternHash>& in_dir_patterns() const;
        [[nodiscard]] const std::unordered_set<std::string>& in_extensions() const;
        [[nodiscard]] const std::unordered_set<RegexPattern, RegexPatternHash>& in_file_patterns() const;
        [[nodiscard]] const std::unordered_set<FileType>& in_file_types() const;
        [[nodiscard]] const std::unordered_set<std::string>& out_archive_extensions() const;
        [[nodiscard]] const std::unordered_set<RegexPattern, RegexPatternHash>& out_archive_file_patterns() const;
        [[nodiscard]] const std::unordered_set<RegexPattern, RegexPatternHash>& out_dir_patterns() const;
        [[nodiscard]] const std::unordered_set<std::string>& out_extensions() const;
        [[nodiscard]] const std::unordered_set<RegexPattern, RegexPatternHash>& out_file_patterns() const;
        [[nodiscard]] const std::unordered_set<FileType>& out_file_types() const;
        [[nodiscard]] const std::unordered_set<std::filesystem::path, PathHash>& paths() const;

        // property setters
        void archives_only(bool archives_only);
        void colorize(bool colorize);
        void debug(bool debug);
        void dir_color(Color dir_color);
        void ext_color(Color ext_color);
        void file_color(Color file_color);
        void follow_symlinks(bool follow_symlinks);
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
#define CONTAINER_SEPARATOR "!"

    class FileResult {
    public:
        FileResult(const std::filesystem::path& file_path, FileType file_type, uint64_t file_size,
                   long last_mod);
        FileResult(const std::vector<std::filesystem::path>& containers, const std::filesystem::path& file_path,
                   FileType file_type, uint64_t file_size, long last_mod);
        [[nodiscard]] std::vector<std::filesystem::path> containers() const;
        [[nodiscard]] std::filesystem::path file_path() const;
        [[nodiscard]] FileType file_type() const;
        [[nodiscard]] uint64_t file_size() const;
        [[nodiscard]] long last_mod() const;
        [[nodiscard]] std::string string() const;
    };

    // FileResultFormatter.h
    class FileResultFormatter {
    public:
        explicit FileResultFormatter(const FindSettings& settings);
        explicit FileResultFormatter(const std::unique_ptr<FindSettings>& settings_ptr);
        FileResultFormatter(FileResultFormatter& other) = delete;
        FileResultFormatter(FileResultFormatter&& other) = delete;
        [[nodiscard]] FindSettings settings() const;
        static std::string colorize(const std::string& s, unsigned long match_start_idx, unsigned long match_end_idx,
            Color color);
        [[nodiscard]] std::string format_dir_path(const std::filesystem::path& dir_path) const;
        [[nodiscard]] std::string format_file_name(const std::string& file_name) const;
        [[nodiscard]] std::string format_file_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] std::string format_file_result(const FileResult& result) const;
    };

    // FileResultSorter.h
    class FileResultSorter {
    public:
        explicit FileResultSorter(const FindSettings& settings);
        explicit FileResultSorter(const std::unique_ptr<FindSettings>& settings_ptr);
        FileResultSorter(FileResultSorter& other) = delete;
        FileResultSorter(FileResultSorter&& other) = delete;
        [[nodiscard]] FindSettings settings() const;
        void sort(std::vector<FileResult>& file_results) const;
    };

    int cmp_file_results_by_path(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_path_ci(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_name(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_name_ci(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_size(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_size_ci(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_type(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_type_ci(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_lastmod(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_lastmod_ci(const FileResult& fr1, const FileResult& fr2);

    // FindOptions.h
    class FindOptions {
    public:
        FindOptions();
        FindSettings settings_from_args(int argc, char **argv);
        void update_settings_from_args(FindSettings& settings, int argc, char **argv);
        void update_settings_from_file(FindSettings& settings, const std::filesystem::path& file_path);
        void update_settings_from_json(FindSettings& settings, std::string_view json_str);
        void usage();
        std::string get_usage_string();
    };

    // Finder.h
    class Finder {
    public:
        // Finder() noexcept;
        explicit Finder(const FindSettings& settings);
        explicit Finder(const std::unique_ptr<FindSettings>& settings_ptr);
        Finder(Finder& other) = delete;
        Finder(Finder&& other) = delete;
        [[nodiscard]] std::optional<FileResult> filter_to_file_result(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_matching_archive_file_result(const FileResult& file_result) const;
        [[nodiscard]] bool filter_dir_path_by_hidden(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool filter_dir_path_by_in_patterns(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool filter_dir_path_by_out_patterns(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool is_matching_dir_path(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool is_matching_archive_extension(const std::string& file_ext) const;
        [[nodiscard]] bool has_matching_archive_extension(const FileResult& file_result) const;
        [[nodiscard]] bool is_matching_extension(const std::string& file_ext) const;
        [[nodiscard]] bool has_matching_extension(const FileResult& file_result) const;
        [[nodiscard]] bool is_matching_archive_file_name(const std::string& file_name) const;
        [[nodiscard]] bool is_matching_file_name(const std::string& file_name) const;
        [[nodiscard]] bool is_matching_file_type(const FileType& file_type) const;
        [[nodiscard]] bool is_matching_file_size(uint64_t file_size) const;
        [[nodiscard]] bool is_matching_last_mod(long last_mod) const;
        [[nodiscard]] bool is_matching_file_result(const FileResult& file_result) const;
        [[nodiscard]] std::vector<FileResult> find() const;
    };

    std::vector<std::filesystem::path> get_matching_dir_paths(const std::vector<FileResult>& file_results);
    void print_file_result_dirs(const std::vector<FileResult>& file_results, const FileResultFormatter& formatter);
    void print_file_results(const std::vector<FileResult>& file_results, const FileResultFormatter& formatter);
}

#endif // CPPFIND_H

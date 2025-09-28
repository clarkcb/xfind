#ifndef CFIND_H
#define CFIND_H

/*
 * cfind.h - the public header file for using cfind as a library dependency
 *
 * This file was created by:
 * 1) concatenating all other header files into a single file
 * 2) removing redundant / unneeded imports
 *
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/stat.h>

#include "regex.h"

#include "rapidjson/document.h"


// common.h

#define MAX_FILENAME_LENGTH 255
#define MAX_PATH_LENGTH 1024
#define MAX_STRING_LENGTH 1024

void log_msg(const char *msg);

void log_err(const char *msg);

bool is_null_or_empty_string(const char *s);

int char_in_string(char c, const char *s);

int char_count_in_string(char c, const char *s);

int index_of_char_in_string(char c, const char *s);

int last_index_of_char_in_string(char c, const char *s);

int index_of_string_in_array(const char *s, char **arr, size_t arr_size);

size_t get_longest_strlen(const char **arr, size_t arr_size);

unsigned int num_digits_ulong(unsigned long num);

void time_to_datestring(long t, char *s);


// config.h

#define MAX_HOMEPATH_LENGTH 100
#define FILE_TYPES_REL_PATH "shared/filetypes.json"
#define FIND_OPTIONS_REL_PATH "shared/findoptions.json"

void get_home_path(char *dest);

void get_xfind_path(char *dest);

void get_file_types_path(char *dest);

void get_find_options_path(char *dest);


// color.h

#define COLOR_RESET  "\033[0m"
#define COLOR_BLACK  "\033[30m"
#define COLOR_RED    "\033[31m"
#define COLOR_GREEN  "\033[32m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_BLUE   "\033[34m"
#define COLOR_PURPLE "\033[35m"
#define COLOR_CYAN   "\033[36m"
#define COLOR_WHITE  "\033[37m"


// finderr.h

/*
 * Error codes - other than E_OK, these start at >300 to be well above the values in
 *               errno.h
 */
#define E_OK                               0 /* OK (no error) */
#define E_UNKNOWN_ERROR                  301 /* Unknown error */
#define E_STARTPATH_NOT_DEFINED          302 /* Startpath not defined */
#define E_STARTPATH_NOT_FOUND            303 /* Startpath not found */
#define E_STARTPATH_NOT_READABLE         304 /* Startpath not readable */
#define E_STARTPATH_STAT_FAILED          305 /* Unable to stat startpath */
#define E_STARTPATH_NON_MATCHING         306 /* Startpath does not match search criteria */
#define E_STARTPATH_UNSUPPORTED_FILETYPE 307 /* Startpath is an unsupported file type */
#define E_INVALID_OPTION                 308 /* Invalid option */
#define E_INVALID_ARG                    309 /* Invalid arg */
#define E_INVALID_ARG_FOR_OPTION         310 /* Missing arg for arg option */
#define E_MISSING_ARG_FOR_OPTION         311 /* Missing arg for arg option */
#define E_DIRECTORY_NOT_FOUND            312 /* Directory not found */
#define E_FILE_NOT_FOUND                 313 /* File not found */
#define E_FILENAME_TOO_LONG              314 /* Filename is too long */
#define E_INVALID_DATESTRING             315 /* Invalid date string (for max_last_mod/min_last_mod) */
#define E_INVALID_DEPTH_RANGE            316 /* Invalid depth range (max_depth < min_depth) */
#define E_INVALID_LASTMOD_RANGE          317 /* Invalid lastmod range (max_last_mod < min_last_mod) */
#define E_INVALID_SIZE_RANGE             318 /* Invalid size range (max_size < min_size) */
#define E_JSON_PARSE_ERROR               319 /* JSON parsing error (invalid on non-JSON) */

typedef unsigned int error_t;

void handle_error(error_t err);


// fileutil.h

#if defined(_WIN32) || defined(_WIN64)
#define OS_WINDOWS 1
#define PATH_SEPARATOR '\\'
#define PATH_SEPARATOR_S "\\"
#else
#define OS_WINDOWS 0
#define PATH_SEPARATOR '/'
#define PATH_SEPARATOR_S "/"
#endif

bool dir_or_file_exists(const char *file_path);

bool dir_or_file_readable(const char *file_path);

bool is_dot_dir(const char *file_path);

long file_size(const char *file_path);

void get_extension(const char *file_name, char *ext);

void get_file_name_without_extension(const char *file_name_with_ext, char *file_name);

bool is_hidden_name(const char *name);

bool is_hidden_path(const char *path);

void expand_path(const char *file_path, char **expanded);

void join_path(const char *path1, const char *path2, char *joined);

void split_path(const char *fp, char** p, char** f);

void normalize_path(char *path);


// intnode.h

typedef struct IntNode {
    const int *integer;
    struct IntNode *next;
} IntNode;

IntNode *empty_int_node(void);

IntNode *new_int_node(const int *i);

void add_int_to_int_node(const int *i, IntNode *int_node);

bool is_null_or_empty_int_node(const IntNode *int_node);

bool int_matches_int_node(const int *i, IntNode *int_node);

size_t int_node_count(IntNode *int_node);

void destroy_int_node(IntNode *int_node);


// pathnode.h

typedef struct Path {
    const char *dir;
    const char *file_name;
} Path;

typedef struct PathNode {
    Path *path;
    struct PathNode *next;
} PathNode;

Path *new_path(const char *file_path);

Path *new_path_from_dir_and_file_name(const char *dir, const char *file_name);

Path *copy_path(const Path *path);

bool is_hidden_path(const Path *path);

int path_cmp(const Path *p1, const Path *p2);

int path_case_cmp(const Path *p1, const Path *p2);

int path_file_name_cmp(const Path *p1, const Path *p2);

int path_file_name_case_cmp(const Path *p1, const Path *p2);

bool path_exists(const Path *path);

bool path_readable(const Path *path);

int path_stat(const Path *path, struct stat *pstat);

size_t path_strlen(const Path *path);

void path_to_string(const Path *path, char *s);

void destroy_path(Path *p);

PathNode *empty_path_node(void);

PathNode *new_path_node(Path *p);

PathNode *new_path_node_from_dir_and_file_name(const char *dir, const char *file_name);

void add_path_to_path_node(Path *p, PathNode *path_node);

void add_dir_and_file_name_to_path_node(const char *dir, const char *file_name, PathNode *path_node);

bool is_null_or_empty_path_node(const PathNode *path_node);

size_t path_node_count(PathNode *path_node);

size_t path_node_strlen(PathNode *path_node);

void path_node_to_string(PathNode *path_node, char *s);

void destroy_path_node(PathNode *path_node);


// regexnode.h

typedef struct Regex {
    const char *pattern;
    regex_t compiled;
} Regex;

typedef struct RegexNode {
    Regex *regex;
    struct RegexNode *next;
} RegexNode;

Regex *new_regex(const char *pat);

RegexNode *new_regex_node(Regex *r);

RegexNode *new_regex_node_from_string(const char *pat);

void add_regex_to_regex_node(Regex *r, RegexNode *regex_node);

void add_string_to_regex_node(const char *pat, RegexNode *regex_node);

bool is_null_or_empty_regex_node(const RegexNode *regex_node);

bool string_matches_regex_node(const char *s, RegexNode *regex_node);

bool string_matches_regex_node_with_matches(const char *s, RegexNode *regex_node, size_t nmatch, regmatch_t *pmatches);

size_t regex_node_count(RegexNode *regex_node);

size_t regex_node_strlen(RegexNode *regex_node);

void regex_node_to_string(RegexNode *regex_node, char *s);

void destroy_regex(Regex *r);

void destroy_regex_node(RegexNode *regex_node);


// stringarray.h

typedef struct StringArray {
    char **strings;
    size_t size;
} StringArray;

StringArray *new_string_array(void);

StringArray *new_string_array_with_size(size_t size);

void add_string_to_string_array(const char *s, StringArray *arr);

size_t string_array_strlen(const StringArray *arr);

int index_of_string_in_string_array(const char *s, const StringArray *arr);

char *string_array_to_string(char *s, StringArray *arr);

void destroy_string_array(StringArray *arr);


// stringnode.h

typedef struct StringNode {
    const char *string;
    struct StringNode *next;
} StringNode;

StringNode *empty_string_node(void);

StringNode *new_string_node(const char *s);

StringNode *new_string_node_from_char_split(char c, const char *s);

void add_string_to_string_node(const char *s, StringNode *string_node);

void add_char_split_to_string_node(char c, const char *s, StringNode *string_node);

bool is_null_or_empty_string_node(const StringNode *string_node);

bool string_matches_string_node(const char *s, const StringNode *string_node);

size_t string_node_count(const StringNode *string_node);

size_t string_node_strlen(const StringNode *string_node);

void string_node_to_string(const StringNode *string_node, char *s);

void destroy_string_node(StringNode *string_node);


// filetypes.h

typedef enum {
    UNKNOWN = 0,
    ARCHIVE = 1,
    AUDIO   = 2,
    BINARY  = 3,
    CODE    = 4,
    FONT    = 5,
    IMAGE   = 6,
    TEXT    = 7,
    VIDEO   = 8,
    XML     = 9
} FileType;


typedef struct FileTypes {
    StringArray *archive_extensions;
    StringArray *archive_names;
    StringArray *audio_extensions;
    StringArray *audio_names;
    StringArray *binary_extensions;
    StringArray *binary_names;
    StringArray *code_extensions;
    StringArray *code_names;
    StringArray *font_extensions;
    StringArray *font_names;
    StringArray *image_extensions;
    StringArray *image_names;
    StringArray *text_extensions;
    StringArray *text_names;
    StringArray *video_extensions;
    StringArray *video_names;
    StringArray *xml_extensions;
    StringArray *xml_names;
} FileTypes;

FileTypes *new_file_types(void);

error_t get_file_types(FileTypes *file_types);

bool is_archive_ext(const char *ext, const FileTypes *file_types);

bool is_archive_name(const char *name, const FileTypes *file_types);

bool is_audio_ext(const char *ext, const FileTypes *file_types);

bool is_audio_name(const char *name, const FileTypes *file_types);

bool is_binary_ext(const char *ext, const FileTypes *file_types);

bool is_binary_name(const char *name, const FileTypes *file_types);

bool is_code_ext(const char *ext, const FileTypes *file_types);

bool is_code_name(const char *name, const FileTypes *file_types);

bool is_font_ext(const char *ext, const FileTypes *file_types);

bool is_font_name(const char *name, const FileTypes *file_types);

bool is_image_ext(const char *ext, const FileTypes *file_types);

bool is_image_name(const char *name, const FileTypes *file_types);

bool is_text_ext(const char *ext, const FileTypes *file_types);

bool is_text_name(const char *name, const FileTypes *file_types);

bool is_video_ext(const char *ext, const FileTypes *file_types);

bool is_video_name(const char *name, const FileTypes *file_types);

bool is_xml_ext(const char *ext, const FileTypes *file_types);

bool is_xml_name(const char *name, const FileTypes *file_types);

FileType get_file_type_for_filename(const char *filename, const FileTypes *file_types);

FileType get_file_type_for_ext(const char *ext, const FileTypes *file_types);

FileType get_file_type(const char *file_name, const FileTypes *file_types);

FileType file_type_from_name(const char *name);

void file_type_to_name(const FileType file_type, char *name);

size_t file_type_node_strlen(IntNode *file_type_node);

void file_type_node_to_string(IntNode *file_type_node, char *s);

void destroy_file_types(FileTypes *file_types);


// findsettings.h

#define BOOLEAN_NAME_FALSE "false"
#define BOOLEAN_NAME_TRUE "true"

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

typedef enum {
    FILEPATH = 0,
    FILENAME = 1,
    FILESIZE = 2,
    FILETYPE = 3,
    LASTMOD  = 4
} SortBy;

typedef struct FindSettings {
    bool archives_only : 1;
    bool colorize : 1;
    bool debug : 1;
    bool follow_symlinks : 1;
    StringNode *in_archive_extensions;
    RegexNode *in_archive_file_patterns;
    RegexNode *in_dir_patterns;
    StringNode *in_extensions;
    RegexNode *in_file_patterns;
    IntNode *in_file_types;
    bool include_archives : 1;
    bool include_hidden : 1;
    int max_depth;
    long max_last_mod;
    unsigned long max_size;
    int min_depth;
    long min_last_mod;
    unsigned long min_size;
    StringNode *out_archive_extensions;
    RegexNode *out_archive_file_patterns;
    RegexNode *out_dir_patterns;
    StringNode *out_extensions;
    RegexNode *out_file_patterns;
    IntNode *out_file_types;
    PathNode *paths;
    bool print_dirs : 1;
    bool print_files : 1;
    bool print_usage : 1;
    bool print_version : 1;
    bool recursive : 1;
    SortBy sort_by;
    bool sort_case_insensitive : 1;
    bool sort_descending : 1;
    bool verbose : 1;
} FindSettings;

FindSettings *default_settings(void);

size_t settings_strlen(const FindSettings *settings);

void settings_to_string(const FindSettings *settings, char *s);

void print_settings(const FindSettings *settings);

void destroy_settings(FindSettings *settings);

void set_archives_only(FindSettings *settings, unsigned short archives_only);

void set_debug(FindSettings *settings, unsigned short debug);

SortBy sort_by_from_name(const char *name);

void sort_by_to_name(const SortBy sort_by, char *name);

bool need_stat(const FindSettings *settings);


// fileresults.h

typedef struct FileResult {
    const Path *path;
    FileType file_type;
    uint64_t file_size;
    long last_mod;
} FileResult;

typedef struct FileResults {
    FileResult *result;
    struct FileResults *next;
} FileResults;

FileResult *new_file_result(const Path *path, FileType file_type, uint64_t file_size, long last_mod);

FileResults *empty_file_results(void);

bool is_null_or_empty_file_results(const FileResults *results);

FileResults *new_file_results(FileResult *r);

void add_to_file_results(FileResult *r, FileResults *results);

size_t file_result_strlen(const FileResult *r);

size_t file_results_count(const FileResults *results);

void file_result_to_string(const FileResult *r, char *s);

void sort_file_result_array(FileResult **arr, size_t n, SortBy sort_by, bool case_insensitive);

void reverse_file_result_array(FileResult *arr[], size_t low, size_t high);

StringNode *dir_results(const FileResults *results);

void print_dir_results(const FileResults *results, const FindSettings *settings);

void print_file_results(const FileResults *results, const FindSettings *settings);

void destroy_file_result(FileResult *r);

void destroy_file_results(FileResults *results);


// findoptions.h

typedef struct FindOption {
    const char *long_arg;
    const char *short_arg;
    const char *description;
} FindOption;

typedef struct FindOptions {
    FindOption *option;
    struct FindOptions *next;
} FindOptions;

typedef enum {
    ARCHIVES_ONLY         = 0,
    COLORIZE              = 1,
    DEBUG                 = 2,
    EXCLUDE_ARCHIVES      = 3,
    EXCLUDE_HIDDEN        = 4,
    FOLLOW_SYMLINKS       = 5,
    INCLUDE_ARCHIVES      = 6,
    INCLUDE_HIDDEN        = 7,
    HELP                  = 8,
    NO_COLORIZE           = 9,
    NO_FOLLOW_SYMLINKS    = 10,
    NO_PRINT_DIRS         = 11,
    NO_PRINT_FILES        = 12,
    NO_RECURSIVE          = 13,
    PRINT_DIRS            = 14,
    PRINT_FILES           = 15,
    RECURSIVE             = 16,
    SORT_ASCENDING        = 17,
    SORT_CASE_INSENSITIVE = 18,
    SORT_CASE_SENSITIVE   = 19,
    SORT_DESCENDING       = 20,
    VERBOSE               = 21,
    VERSION               = 22
} SettingsBoolType;

typedef enum {
    IN_ARCHIVE_EXTENSION     = 0,
    IN_ARCHIVE_FILE_PATTERN  = 1,
    IN_DIR_PATTERN           = 2,
    IN_EXTENSION             = 3,
    IN_FILE_PATTERN          = 4,
    IN_FILE_TYPE             = 5,
    MAX_LAST_MOD             = 6,
    MIN_LAST_MOD             = 7,
    OUT_ARCHIVE_EXT          = 8,
    OUT_ARCHIVE_FILE_PATTERN = 9,
    OUT_DIR_PATTERN          = 10,
    OUT_EXTENSION            = 11,
    OUT_FILE_PATTERN         = 12,
    OUT_FILE_TYPE            = 13,
    PATH                     = 14,
    SETTINGS_FILE            = 15,
    SORT_BY                  = 16
} SettingsStringType;

typedef enum {
    MAX_DEPTH                = 0,
    MIN_DEPTH                = 1
} SettingsIntType;

typedef enum {
    MAX_SIZE                 = 0,
    MIN_SIZE                 = 1
} SettingsLongType;

FindOption *new_find_option(const char *long_arg, const char *short_arg, const char *desc);

FindOptions *empty_find_options(void);

FindOptions *new_find_options(FindOption *o);

void add_to_find_options(FindOption *o, FindOptions *options);

error_t get_find_options(FindOptions *options);

error_t settings_from_args(const int argc, char *argv[], FindSettings *settings);

error_t settings_from_json_string(const char *settings_json_str, FindSettings *settings);

error_t settings_from_json_file(const char *settings_json_file_path, FindSettings *settings);

size_t find_options_count(FindOptions *options);

size_t find_option_usage_strlen(FindOption *o, size_t longest_opt_len);

size_t find_options_usage_strlen(FindOptions *options);

void find_options_to_usage_string(FindOptions *options, char *s);

void print_usage(void);

void destroy_find_option(FindOption *o);

void destroy_find_options(FindOptions *options);


// finder.h

typedef struct Finder {
    FindSettings *settings;
    FileTypes *file_types;
} Finder;

Finder *new_finder(const FindSettings *s, const FileTypes *ft);

error_t validate_settings(const FindSettings *settings);

bool is_matching_dir(const FindSettings *settings, const char *dir);

bool is_matching_path(const FindSettings *settings, const Path *path,
                      const FileType *file_type, uint64_t file_size, long last_mod);

bool filter_path(const FindSettings *settings, const Path *path,
                 const FileType *file_type, uint64_t file_size, long last_mod);

error_t filter_to_file_results(const Finder *finder, const PathNode *file_paths, FileResults *results);

error_t filter_paths_to_file_results(const Finder *finder, const PathNode *file_paths, FileResults *results);

error_t find(const FindSettings *settings, FileResults *results);

void destroy_finder(Finder *finder);


#endif // CFIND_H

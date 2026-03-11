#ifndef FINDOPTIONS_H
#define FINDOPTIONS_H

#include <errno.h>
#include <stddef.h>

#include "findsettings.h"
#include "options.h"

// semantic aliases (they're equivalent to the Option/Options versions
typedef Option FindOption;
typedef Options FindOptions;

typedef enum {
    ARCHIVES_ONLY         = 0,
    COLORIZE              = 1,
    DEBUG                 = 2,
    DEFAULT_FILES         = 3,
    EXCLUDE_ARCHIVES      = 4,
    EXCLUDE_HIDDEN        = 5,
    FOLLOW_SYMLINKS       = 6,
    INCLUDE_ARCHIVES      = 7,
    INCLUDE_HIDDEN        = 8,
    HELP                  = 9,
    NO_COLORIZE           = 10,
    NO_DEFAULT_FILES      = 11,
    NO_FOLLOW_SYMLINKS    = 12,
    NO_PRINT_DIRS         = 13,
    NO_PRINT_FILES        = 14,
    NO_RECURSIVE          = 15,
    PRINT_DIRS            = 16,
    PRINT_FILES           = 17,
    RECURSIVE             = 18,
    SORT_ASCENDING        = 19,
    SORT_CASE_INSENSITIVE = 20,
    SORT_CASE_SENSITIVE   = 21,
    SORT_DESCENDING       = 22,
    VERBOSE               = 23,
    VERSION               = 24
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

FindOption *new_find_option(const char *long_arg, const char *short_arg, const char *desc, int arg_type);

FindOptions *empty_find_options(void);

FindOptions *new_find_options(FindOption *o);

void add_to_find_options(FindOption *o, FindOptions *options);

error_t get_find_options(FindOptions *options);

error_t settings_from_args(int argc, char *argv[], FindOptions *options, FindSettings *settings);

error_t settings_from_json_string(const char *settings_json_str, FindOptions *options, FindSettings *settings);

error_t settings_from_json_file(const char *settings_json_file_path, FindOptions *options, FindSettings *settings);

error_t settings_from_default_files(FindOptions *options, FindSettings *settings);

size_t find_options_count(FindOptions *options);

size_t find_option_usage_strlen(const FindOption *o, size_t longest_opt_len);

size_t find_options_usage_strlen(FindOptions *options);

void find_options_to_usage_string(FindOptions *options, char *s);

void print_usage_for_options(FindOptions *options);

void print_usage(void);

void destroy_find_option(FindOption *o);

void destroy_find_options(FindOptions *options);

#endif

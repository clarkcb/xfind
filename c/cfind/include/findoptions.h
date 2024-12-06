#ifndef FINDOPTIONS_H
#define FINDOPTIONS_H

#include "findsettings.h"

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
    DEBUG                 = 1,
    EXCLUDE_ARCHIVES      = 2,
    EXCLUDE_HIDDEN        = 3,
    FOLLOW_SYMLINKS       = 4,
    INCLUDE_ARCHIVES      = 5,
    INCLUDE_HIDDEN        = 6,
    HELP                  = 7,
    NO_FOLLOW_SYMLINKS    = 8,
    NO_PRINT_DIRS         = 9,
    NO_PRINT_FILES        = 10,
    NO_RECURSIVE          = 11,
    PRINT_DIRS            = 12,
    PRINT_FILES           = 13,
    RECURSIVE             = 14,
    SORT_ASCENDING        = 15,
    SORT_CASE_INSENSITIVE = 16,
    SORT_CASE_SENSITIVE   = 17,
    SORT_DESCENDING       = 18,
    VERBOSE               = 19,
    VERSION               = 20
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

#endif

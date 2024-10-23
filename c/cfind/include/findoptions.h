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
    IN_ARCHIVE_EXTENSION     = 0,
    IN_ARCHIVE_FILE_PATTERN  = 1,
    IN_DIR_PATTERN           = 2,
    IN_EXTENSION             = 3,
    IN_FILE_PATTERN          = 4,
    IN_FILE_TYPE             = 5,
    MAX_DEPTH                = 6,
    MAX_LAST_MOD             = 7,
    MAX_SIZE                 = 8,
    MIN_DEPTH                = 9,
    MIN_LAST_MOD             = 10,
    MIN_SIZE                 = 11,
    OUT_ARCHIVE_EXT          = 12,
    OUT_ARCHIVE_FILE_PATTERN = 13,
    OUT_DIR_PATTERN          = 14,
    OUT_EXTENSION            = 15,
    OUT_FILE_PATTERN         = 16,
    OUT_FILE_TYPE            = 17,
    PATH                     = 18,
    SETTINGS_FILE            = 19,
    SORT_BY                  = 20
} SettingsCollType;

typedef enum {
    ARCHIVES_ONLY         = 0,
    DEBUG                 = 1,
    EXCLUDE_ARCHIVES      = 2,
    EXCLUDE_HIDDEN        = 3,
    INCLUDE_ARCHIVES      = 4,
    INCLUDE_HIDDEN        = 5,
    HELP                  = 6,
    NO_PRINT_DIRS         = 7,
    NO_PRINT_FILES        = 8,
    NO_RECURSIVE          = 9,
    PRINT_DIRS            = 10,
    PRINT_FILES           = 11,
    RECURSIVE             = 12,
    SORT_ASCENDING        = 13,
    SORT_CASE_INSENSITIVE = 14,
    SORT_CASE_SENSITIVE   = 15,
    SORT_DESCENDING       = 16,
    VERBOSE               = 17,
    VERSION               = 18
} SettingsFlagType;

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

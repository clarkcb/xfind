#ifndef FINDOPTIONS_H
#define FINDOPTIONS_H

#include "findsettings.h"

typedef struct FindOption {
    const char *longarg;
    const char *shortarg;
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
    MAX_LAST_MOD             = 6,
    MAX_SIZE                 = 7,
    MIN_LAST_MOD             = 8,
    MIN_SIZE                 = 9,
    OUT_ARCHIVE_EXT          = 10,
    OUT_ARCHIVE_FILE_PATTERN = 11,
    OUT_DIR_PATTERN          = 12,
    OUT_EXTENSION            = 13,
    OUT_FILE_PATTERN         = 14,
    OUT_FILE_TYPE            = 15,
    PATH                     = 16,
    SORT_BY                  = 17
} SettingsCollType;

typedef enum {
    ARCHIVES_ONLY         = 0,
    DEBUG                 = 1,
    EXCLUDE_ARCHIVES      = 2,
    EXCLUDE_HIDDEN        = 3,
    INCLUDE_ARCHIVES      = 4,
    INCLUDE_HIDDEN        = 5,
    HELP                  = 6,
    LIST_DIRS             = 7,
    LIST_FILES            = 8,
    NO_RECURSIVE          = 9,
    RECURSIVE             = 10,
    SORT_ASCENDING        = 11,
    SORT_CASE_INSENSITIVE = 12,
    SORT_CASE_SENSITIVE   = 13,
    SORT_DESCENDING       = 14,
    VERBOSE               = 15,
    VERSION               = 16
} SettingsFlagType;

FindOption *new_find_option(const char *longarg, const char *shortarg, const char *desc);

FindOptions *empty_find_options(void);

FindOptions *new_find_options(FindOption *o);

void add_to_find_options(FindOption *o, FindOptions *options);

error_t get_find_options(FindOptions *options);

error_t settings_from_args(const int argc, char *argv[], FindSettings *settings);

size_t find_options_count(FindOptions *options);

size_t find_option_usage_strlen(FindOption *o, size_t longest_opt_len);

size_t find_options_usage_strlen(FindOptions *options);

void find_options_to_usage_string(FindOptions *options, char *s);

void print_usage(void);

void destroy_find_option(FindOption *o);

void destroy_find_options(FindOptions *options);

#endif

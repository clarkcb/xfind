#ifndef FINDSETTINGS_H
#define FINDSETTINGS_H

#include "intnode.h"
#include "regexnode.h"
#include "stringnode.h"

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
    unsigned short archives_only : 1;
    unsigned short debug : 1;
    StringNode *in_archive_extensions;
    RegexNode *in_archive_file_patterns;
    RegexNode *in_dir_patterns;
    StringNode *in_extensions;
    RegexNode *in_file_patterns;
    IntNode *in_file_types;
    unsigned short include_archives : 1;
    unsigned short include_hidden : 1;
    unsigned short list_dirs : 1;
    unsigned short list_files : 1;
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
    StringNode *paths;
    unsigned short print_usage : 1;
    unsigned short print_version : 1;
    unsigned short recursive : 1;
    SortBy sort_by;
    unsigned short sort_case_insensitive : 1;
    unsigned short sort_descending : 1;
    unsigned short verbose : 1;
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

#endif

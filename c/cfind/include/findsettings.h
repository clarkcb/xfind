#ifndef FINDSETTINGS_H
#define FINDSETTINGS_H

#include "intnode.h"
#include "regexnode.h"
#include "stringnode.h"

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
    unsigned short exclude_hidden : 1;
    StringNode *in_archive_extensions;
    RegexNode *in_archive_file_patterns;
    RegexNode *in_dir_patterns;
    StringNode *in_extensions;
    RegexNode *in_file_patterns;
    IntNode *in_file_types;
    unsigned short include_archives : 1;
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
    unsigned short print_results : 1;
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

SortBy sort_by_from_name(const char *name);

void sort_by_to_name(const SortBy sort_by, char *name);

#endif

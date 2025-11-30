#ifndef FINDSETTINGS_H
#define FINDSETTINGS_H

#include <stdbool.h>

#include "color.h"
#include "intnode.h"
#include "pathnode.h"
#include "regexnode.h"
#include "sortby.h"
#include "stringnode.h"

#define BOOLEAN_NAME_FALSE "false"
#define BOOLEAN_NAME_TRUE "true"

typedef struct FindSettings {
    bool archives_only : 1;
    bool colorize : 1;
    bool debug : 1;
    Color dir_color;
    Color ext_color;
    Color file_color;
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

bool need_stat(const FindSettings *settings);

#endif

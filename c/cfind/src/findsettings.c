#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "filetypes.h"
#include "findsettings.h"

unsigned int num_digits_ulong(unsigned long num);

FindSettings *default_settings(void)
{
    FindSettings *settings = malloc(sizeof(FindSettings));
    assert(settings != NULL);

    settings->archives_only = 0;
    settings->debug = 0;
    settings->in_archive_extensions = NULL;
    settings->in_archive_file_patterns = NULL;
    settings->in_dir_patterns = NULL;
    settings->in_extensions = NULL;
    settings->in_file_patterns = NULL;
    settings->in_file_types = NULL;
    settings->include_archives = 0;
    settings->include_hidden = 0;
    settings->max_depth = -1;
    settings->max_last_mod = 0L;
    settings->max_size = 0L;
    settings->min_depth = -1;
    settings->min_last_mod = 0L;
    settings->min_size = 0L;
    settings->out_archive_extensions = NULL;
    settings->out_archive_file_patterns = NULL;
    settings->out_dir_patterns = NULL;
    settings->out_extensions = NULL;
    settings->out_file_patterns = NULL;
    settings->out_file_types = NULL;
    settings->paths = NULL;
    settings->print_dirs = 0;
    settings->print_files = 0;
    settings->print_usage = 0;
    settings->print_version = 0;
    settings->recursive = 1;
    settings->sort_by = FILEPATH;
    settings->sort_case_insensitive = 0;
    settings->sort_descending = 0;
    settings->verbose = 0;

    return settings;
}

const int SETTINGS_BOOL_FIELD_COUNT = 13;
const int SETTINGS_LONG_FIELD_COUNT = 4;
const int SETTINGS_STRING_NODE_FIELD_COUNT = 13;
const int SETTINGS_SORTBY_FIELD_COUNT = 1;
const int SETTINGS_TOTAL_FIELD_COUNT = SETTINGS_BOOL_FIELD_COUNT + SETTINGS_LONG_FIELD_COUNT +
	SETTINGS_STRING_NODE_FIELD_COUNT + SETTINGS_SORTBY_FIELD_COUNT;
const char *SETTINGS_TEMPLATE = "FindSettings("
            "archives_only=%s"
            ", debug=%s"
            ", in_archive_extensions=%s"
            ", in_archive_file_patterns=%s"
            ", in_dir_patterns=%s"
            ", in_extensions=%s"
            ", in_file_patterns=%s"
            ", in_file_types=%s"
            ", include_archives=%s"
            ", include_hidden=%s"
            ", max_depth=%d"
            ", max_last_mod=%s"
            ", max_size=%lu"
            ", min_depth=%d"
            ", min_last_mod=%s"
            ", min_size=%lu"
            ", out_archive_extensions=%s"
            ", out_archive_file_patterns=%s"
            ", out_dir_patterns=%s"
            ", out_extensions=%s"
            ", out_file_patterns=%s"
            ", out_file_types=%s"
            ", paths=%s"
            ", print_dirs=%s"
            ", print_files=%s"
            ", print_usage=%s"
            ", print_version=%s"
            ", recursive=%s"
            ", sort_by=%s"
            ", sort_case_insensitive=%s"
            ", sort_descending=%s"
            ", verbose=%s"
            ")";

static size_t all_bools_strlen(const FindSettings *settings)
{
    return
            (settings->archives_only == 0 ? 5 : 4) +
            (settings->debug == 0 ? 5 : 4) +
            (settings->include_archives == 0 ? 5 : 4) +
            (settings->include_hidden == 0 ? 5 : 4) +
            (settings->print_dirs == 0 ? 5 : 4) +
            (settings->print_files == 0 ? 5 : 4) +
            (settings->print_usage == 0 ? 5 : 4) +
            (settings->print_version == 0 ? 5 : 4) +
            (settings->recursive == 0 ? 5 : 4) +
            (settings->sort_case_insensitive == 0 ? 5 : 4) +
            (settings->sort_descending == 0 ? 5 : 4) +
            (settings->verbose == 0 ? 5 : 4);
}

static size_t all_strings_strlen(const FindSettings *settings)
{
    size_t sortby_strlen = 10;
    return
            string_node_strlen(settings->in_archive_extensions) +
            regex_node_strlen(settings->in_archive_file_patterns) +
            regex_node_strlen(settings->in_dir_patterns) +
            string_node_strlen(settings->in_extensions) +
            regex_node_strlen(settings->in_file_patterns) +
            file_type_node_strlen(settings->in_file_types) +
            string_node_strlen(settings->out_archive_extensions) +
            regex_node_strlen(settings->out_archive_file_patterns) +
            regex_node_strlen(settings->out_dir_patterns) +
            string_node_strlen(settings->out_extensions) +
            regex_node_strlen(settings->out_file_patterns) +
            file_type_node_strlen(settings->out_file_types) +
            sortby_strlen +
            string_node_strlen(settings->paths);
}

static size_t last_mod_strlen(long last_mod)
{
    return last_mod == 0 ? 1 : 12; // 10 for "yyyy-MM-dd" plus 2 for surrounding double quotes
}

size_t settings_strlen(const FindSettings *settings)
{
    return strnlen(SETTINGS_TEMPLATE, 1024)
        - (SETTINGS_TOTAL_FIELD_COUNT * 2 + 2) // the + 2 is for the extra formatting letter for each long (%lu)
        + all_bools_strlen(settings)
        + last_mod_strlen(settings->max_last_mod)
        + num_digits_ulong(settings->max_size)
        + last_mod_strlen(settings->min_last_mod)
        + num_digits_ulong(settings->min_size)
        + all_strings_strlen(settings);
}

void settings_to_string(const FindSettings *settings, char *s)
{
    // assumes s has correct allocation size
    char *archives_only_s = settings->archives_only == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    char *in_archive_extensions_s = malloc(string_node_strlen(settings->in_archive_extensions) + 1);
    in_archive_extensions_s[0] = '\0';
    string_node_to_string(settings->in_archive_extensions, in_archive_extensions_s);
    char *in_archive_file_patterns_s = malloc(regex_node_strlen(settings->in_archive_file_patterns) + 1);
    in_archive_file_patterns_s[0] = '\0';
    regex_node_to_string(settings->in_archive_file_patterns, in_archive_file_patterns_s);
    char *debug_s = settings->debug == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    char *in_dir_patterns_s = malloc(regex_node_strlen(settings->in_dir_patterns) + 1);
    in_dir_patterns_s[0] = '\0';
    regex_node_to_string(settings->in_dir_patterns, in_dir_patterns_s);
    char *in_extensions_s = malloc(string_node_strlen(settings->in_extensions) + 1);
    in_extensions_s[0] = '\0';
    string_node_to_string(settings->in_extensions, in_extensions_s);
    char *in_file_patterns_s = malloc(regex_node_strlen(settings->in_file_patterns) + 1);
    in_file_patterns_s[0] = '\0';
    regex_node_to_string(settings->in_file_patterns, in_file_patterns_s);
    char *in_file_types_s = malloc(file_type_node_strlen(settings->in_file_types) + 1);
    in_file_types_s[0] = '\0';
    file_type_node_to_string(settings->in_file_types, in_file_types_s);
    char *include_archives_s = settings->include_archives == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    char *include_hidden_s = settings->include_hidden == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;

    size_t max_last_mod_strlen = last_mod_strlen(settings->max_last_mod);
    char *max_last_mod_s = malloc(max_last_mod_strlen + 1);
    max_last_mod_s[0] = '\0';
    time_to_datestring(settings->max_last_mod, max_last_mod_s);
    if (max_last_mod_strlen > 1) {
        char *quoted_max_last_mod_s = malloc(max_last_mod_strlen + 3);
        sprintf(quoted_max_last_mod_s, "\"%s\"", max_last_mod_s);
        free(max_last_mod_s);
        max_last_mod_s = quoted_max_last_mod_s;
    }

    size_t min_last_mod_strlen = last_mod_strlen(settings->min_last_mod);
    char *min_last_mod_s = malloc(min_last_mod_strlen + 1);
    min_last_mod_s[0] = '\0';
    time_to_datestring(settings->min_last_mod, min_last_mod_s);
    if (min_last_mod_strlen > 1) {
        char *quoted_min_last_mod_s = malloc(min_last_mod_strlen + 3);
        sprintf(quoted_min_last_mod_s, "\"%s\"", min_last_mod_s);
        free(min_last_mod_s);
        min_last_mod_s = quoted_min_last_mod_s;
    }

    char *out_archive_extensions_s = malloc(string_node_strlen(settings->out_archive_extensions) + 1);
    out_archive_extensions_s[0] = '\0';
    string_node_to_string(settings->out_archive_extensions, out_archive_extensions_s);
    char *out_archive_file_patterns_s = malloc(regex_node_strlen(settings->out_archive_file_patterns) + 1);
    out_archive_file_patterns_s[0] = '\0';
    regex_node_to_string(settings->out_archive_file_patterns, out_archive_file_patterns_s);
    char *out_dir_patterns_s = malloc(regex_node_strlen(settings->out_dir_patterns) + 1);
    out_dir_patterns_s[0] = '\0';
    regex_node_to_string(settings->out_dir_patterns, out_dir_patterns_s);
    char *out_extensions_s = malloc(string_node_strlen(settings->out_extensions) + 1);
    out_extensions_s[0] = '\0';
    string_node_to_string(settings->out_extensions, out_extensions_s);
    char *out_file_patterns_s = malloc(regex_node_strlen(settings->out_file_patterns) + 1);
    out_file_patterns_s[0] = '\0';
    regex_node_to_string(settings->out_file_patterns, out_file_patterns_s);
    char *out_file_types_s = malloc(file_type_node_strlen(settings->out_file_types) + 1);
    out_file_types_s[0] = '\0';
    file_type_node_to_string(settings->out_file_types, out_file_types_s);
    char *paths_s = malloc(string_node_strlen(settings->paths) + 1);
    paths_s[0] = '\0';
    string_node_to_string(settings->paths, paths_s);
    const char *print_dirs_s = settings->print_dirs == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    const char *print_files_s = settings->print_files == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    const char *print_usage_s = settings->print_usage == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    const char *print_version_s = settings->print_version == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    const char *recursive_s = settings->recursive == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    char *sort_by_name = malloc(10 * sizeof(char));
    sort_by_to_name(settings->sort_by, sort_by_name);
    const char *sort_case_insensitive_s = settings->sort_case_insensitive == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    const char *sort_descending_s = settings->sort_descending == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;
    const char *verbose_s = settings->verbose == 0 ? BOOLEAN_NAME_FALSE : BOOLEAN_NAME_TRUE;


    sprintf(s, SETTINGS_TEMPLATE,
        archives_only_s,
        debug_s,
        in_archive_extensions_s,
        in_archive_file_patterns_s,
        in_dir_patterns_s,
        in_extensions_s,
        in_file_patterns_s,
        in_file_types_s,
        include_archives_s,
        include_hidden_s,
        settings->max_depth,
        max_last_mod_s,
        settings->max_size,
        settings->min_depth,
        min_last_mod_s,
        settings->min_size,
        out_archive_extensions_s,
        out_archive_file_patterns_s,
        out_dir_patterns_s,
        out_extensions_s,
        out_file_patterns_s,
        out_file_types_s,
        paths_s,
        print_dirs_s,
        print_files_s,
        print_usage_s,
        print_version_s,
        recursive_s,
        sort_by_name,
        sort_case_insensitive_s,
        sort_descending_s,
        verbose_s);

    size_t total_len = settings_strlen(settings) + 1;

    s[total_len - 1] = '\0';

    free(in_archive_extensions_s);
    free(in_archive_file_patterns_s);
    free(in_dir_patterns_s);
    free(in_extensions_s);
    free(in_file_patterns_s);
    free(in_file_types_s);
    free(max_last_mod_s);
    free(min_last_mod_s);
    free(out_archive_extensions_s);
    free(out_archive_file_patterns_s);
    free(out_dir_patterns_s);
    free(out_extensions_s);
    free(out_file_patterns_s);
    free(out_file_types_s);
    free(paths_s);
    free(sort_by_name);
}

void print_settings(const FindSettings *settings)
{
    size_t settings_len = settings_strlen(settings);
    char ss[settings_len];
    settings_to_string(settings, ss);
    char debug_str[11 + settings_len];
    sprintf(debug_str, "settings: %s", ss);
    log_msg(debug_str);
}

void destroy_settings(FindSettings *settings)
{
    if (settings != NULL) {
        destroy_string_node(settings->in_archive_extensions);
        destroy_regex_node(settings->in_archive_file_patterns);
        destroy_regex_node(settings->in_dir_patterns);
        destroy_string_node(settings->in_extensions);
        destroy_regex_node(settings->in_file_patterns);
        destroy_int_node(settings->in_file_types);
        destroy_string_node(settings->out_archive_extensions);
        destroy_regex_node(settings->out_archive_file_patterns);
        destroy_regex_node(settings->out_dir_patterns);
        destroy_string_node(settings->out_extensions);
        destroy_regex_node(settings->out_file_patterns);
        destroy_int_node(settings->out_file_types);
        destroy_string_node(settings->paths);
        free(settings);
    }
}

void set_archives_only(FindSettings *settings, const unsigned short archives_only)
{
    settings->archives_only = archives_only;
    if (archives_only) {
        settings->include_archives = 1;
    }
}

void set_debug(FindSettings *settings, const unsigned short debug)
{
    settings->debug = debug;
    if (debug) {
        settings->verbose = 1;
    }
}

SortBy sort_by_from_name(const char *name)
{
    //printf("name: %s\n", name);
    size_t maxlen = 10;
    size_t namelen = strnlen(name, maxlen);
    size_t minlen = namelen < maxlen  ? namelen : maxlen;
    char lname[10] = {0};
    strncpy(lname, name, minlen);
    //printf("namelen: %zu\n", namelen);
    //printf("minlen: %zu\n", minlen);
    for (int i = 0; i < minlen; i++) {
        char c = (char)tolower(name[i]);
        lname[i] = c;
    }
    //printf("uname: %s\n", uname);
    if (strncmp(lname, SORT_BY_NAME_FILENAME, maxlen) == 0 || strncmp(lname, SORT_BY_NAME_NAME, maxlen) == 0) {
        return FILENAME;
    }
    if (strncmp(lname, SORT_BY_NAME_FILESIZE, maxlen) == 0 || strncmp(lname, SORT_BY_NAME_SIZE, maxlen) == 0) {
        return FILESIZE;
    }
    if (strncmp(lname, SORT_BY_NAME_FILETYPE, maxlen) == 0 || strncmp(lname, SORT_BY_NAME_TYPE, maxlen) == 0) {
        return FILETYPE;
    }
    if (strncmp(lname, SORT_BY_NAME_LASTMOD, maxlen) == 0) {
        return LASTMOD;
    }
    return FILEPATH;
}

void sort_by_to_name(const SortBy sort_by, char *name)
{
    switch(sort_by) {
        case FILEPATH:
            strncpy(name, SORT_BY_NAME_FILEPATH, 8);
            name[8] = '\0';
            break;
        case FILENAME:
            strncpy(name, SORT_BY_NAME_FILENAME, 8);
            name[8] = '\0';
            break;
        case FILESIZE:
            strncpy(name, SORT_BY_NAME_FILESIZE, 8);
            name[8] = '\0';
            break;
        case FILETYPE:
            strncpy(name, SORT_BY_NAME_FILETYPE, 8);
            name[8] = '\0';
            break;
        case LASTMOD:
            strncpy(name, SORT_BY_NAME_LASTMOD, 7);
            name[7] = '\0';
            break;
        default:
            strncpy(name, SORT_BY_NAME_UNKNOWN, 7);
            name[7] = '\0';
    }
}

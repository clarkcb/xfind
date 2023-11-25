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
    settings->list_dirs = 0;
    settings->list_files = 0;
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
    settings->print_results = 1;
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
const int SETTINGS_TOTAL_FIELD_COUNT = SETTINGS_BOOL_FIELD_COUNT + SETTINGS_LONG_FIELD_COUNT + SETTINGS_STRING_NODE_FIELD_COUNT + SETTINGS_SORTBY_FIELD_COUNT;
const char *SETTINGS_TEMPLATE = "FindSettings("
            "archives_only=%d"
            ", debug=%d"
            ", in_archive_extensions=%s"
            ", in_archive_file_patterns=%s"
            ", in_dir_patterns=%s"
            ", in_extensions=%s"
            ", in_file_patterns=%s"
            ", in_file_types=%s"
            ", include_archives=%d"
            ", include_hidden=%d"
            ", list_dirs=%d"
            ", list_files=%d"
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
            ", print_results=%d"
            ", print_usage=%d"
            ", print_version=%d"
            ", recursive=%d"
            ", sort_by=%s"
            ", sort_case_insensitive=%d"
            ", sort_descending=%d"
            ", verbose=%d"
            ")";

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
    return last_mod == 0 ? 1 : 10;
}

size_t settings_strlen(const FindSettings *settings)
{
    size_t max_last_mod_strlen = last_mod_strlen(settings->max_last_mod);
    if (max_last_mod_strlen > 1) max_last_mod_strlen += 2; // for surrounding double quotes
    size_t min_last_mod_strlen = last_mod_strlen(settings->min_last_mod);
    if (min_last_mod_strlen > 1) min_last_mod_strlen += 2; // for surrounding double quotes

    return strlen(SETTINGS_TEMPLATE)
        - (SETTINGS_TOTAL_FIELD_COUNT * 2 + 4) // the + 4 is for the extra formatting letter for each long (%lu)
        + SETTINGS_BOOL_FIELD_COUNT
        + max_last_mod_strlen
        + num_digits_ulong(settings->max_size)
        + min_last_mod_strlen
        + num_digits_ulong(settings->min_size)
        + all_strings_strlen(settings);
}

void settings_to_string(const FindSettings *settings, char *s)
{
    // assumes s has correct allocation size
    char *in_archive_extensions_s = malloc(string_node_strlen(settings->in_archive_extensions) + 1);
    in_archive_extensions_s[0] = '\0';
    string_node_to_string(settings->in_archive_extensions, in_archive_extensions_s);
    char *in_archive_file_patterns_s = malloc(regex_node_strlen(settings->in_archive_file_patterns) + 1);
    in_archive_file_patterns_s[0] = '\0';
    regex_node_to_string(settings->in_archive_file_patterns, in_archive_file_patterns_s);
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
    char *sort_by_name = malloc(10 * sizeof(char));
    sort_by_to_name(settings->sort_by, sort_by_name);


    sprintf(s, SETTINGS_TEMPLATE,
        settings->archives_only,
        settings->debug,
        in_archive_extensions_s,
        in_archive_file_patterns_s,
        in_dir_patterns_s,
        in_extensions_s,
        in_file_patterns_s,
        in_file_types_s,
        settings->include_archives,
        settings->include_hidden,
        settings->list_dirs,
        settings->list_files,
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
        settings->print_results,
        settings->print_usage,
        settings->print_version,
        settings->recursive,
        sort_by_name,
        settings->sort_case_insensitive,
        settings->sort_descending,
        settings->verbose);

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
    char debug_str[11 + strlen(ss)];
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
    size_t namelen = strlen(name);
    size_t minlen = maxlen < namelen ? maxlen : namelen;
    char uname[10] = {0};
    strncpy(uname, name, minlen);
    //printf("namelen: %zu\n", namelen);
    //printf("minlen: %zu\n", minlen);
    for (int i = 0; i < minlen; i++) {
        char c = (char)toupper(name[i]);
        uname[i] = c;
    }
    //printf("uname: %s\n", uname);
    if (strncmp(uname, "NAME", maxlen) == 0) {
        return FILENAME;
    }
    if (strncmp(uname, "PATH", maxlen) == 0) {
        return FILEPATH;
    }
    if (strncmp(uname, "SIZE", maxlen) == 0) {
        return FILESIZE;
    }
    if (strncmp(uname, "TYPE", maxlen) == 0) {
        return FILETYPE;
    }
    if (strncmp(uname, "LASTMOD", maxlen) == 0) {
        return LASTMOD;
    }
    return FILEPATH;
}

void sort_by_to_name(const SortBy sort_by, char *name)
{
    switch(sort_by) {
        case FILEPATH:
            strncpy(name, "FILEPATH", 8);
            name[8] = '\0';
            break;
        case FILENAME:
            strncpy(name, "FILENAME", 8);
            name[8] = '\0';
            break;
        case FILESIZE:
            strncpy(name, "FILESIZE", 8);
            name[8] = '\0';
            break;
        case FILETYPE:
            strncpy(name, "FILETYPE", 8);
            name[8] = '\0';
            break;
        case LASTMOD:
            strncpy(name, "LASTMOD", 7);
            name[7] = '\0';
            break;
        default:
            strncpy(name, "UNKNOWN", 7);
            name[7] = '\0';
    }
}

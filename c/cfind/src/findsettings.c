#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "filetypes.h"
#include "findsettings.h"

FindSettings *default_settings(void)
{
    FindSettings *settings = malloc(sizeof(FindSettings));
    assert(settings != NULL);

    settings->archivesonly = 0;
    settings->colorize = 1;
    settings->debug = 0;
    settings->excludehidden = 1;
    settings->in_archiveextensions = NULL;
    settings->in_archivefilepatterns = NULL;
    settings->in_dirpatterns = NULL;
    settings->in_extensions = NULL;
    settings->in_filepatterns = NULL;
    settings->in_filetypes = NULL;
    settings->includearchives = 0;
    settings->listdirs = 0;
    settings->listfiles = 0;
    settings->out_archiveextensions = NULL;
    settings->out_archivefilepatterns = NULL;
    settings->out_dirpatterns = NULL;
    settings->out_extensions = NULL;
    settings->out_filepatterns = NULL;
    settings->out_filetypes = NULL;
    settings->paths = NULL;
    settings->printresults = 1;
    settings->printusage = 0;
    settings->printversion = 0;
    settings->recursive = 1;
    settings->verbose = 0;

    return settings;
}

const int SETTINGS_BOOL_FIELD_COUNT = 12;
const int SETTINGS_STRING_NODE_FIELD_COUNT = 13;
const int SETTINGS_TOTAL_FIELD_COUNT = SETTINGS_BOOL_FIELD_COUNT + SETTINGS_STRING_NODE_FIELD_COUNT;
const char *SETTINGS_TEMPLATE = "FindSettings("
            "archivesonly=%d"
            ", colorize=%d"
            ", debug=%d"
            ", excludehidden=%d"
            ", in_archiveextensions=%s"
            ", in_archivefilepatterns=%s"
            ", in_dirpatterns=%s"
            ", in_extensions=%s"
            ", in_filepatterns=%s"
            ", in_filetypes=%s"
            ", includearchives=%d"
            ", listdirs=%d"
            ", listfiles=%d"
            ", out_archiveextensions=%s"
            ", out_archivefilepatterns=%s"
            ", out_dirpatterns=%s"
            ", out_extensions=%s"
            ", out_filepatterns=%s"
            ", out_filetypes=%s"
            ", paths=%s"
            ", printresults=%d"
            ", printusage=%d"
            ", printversion=%d"
            ", recursive=%d"
            ", verbose=%d"
            ")";

static size_t all_strings_strlen(FindSettings *settings)
{
    return
        string_node_strlen(settings->in_archiveextensions) +
        regex_node_strlen(settings->in_archivefilepatterns) +
        regex_node_strlen(settings->in_dirpatterns) +
        string_node_strlen(settings->in_extensions) +
        regex_node_strlen(settings->in_filepatterns) +
        filetype_node_strlen(settings->in_filetypes) +
        string_node_strlen(settings->out_archiveextensions) +
        regex_node_strlen(settings->out_archivefilepatterns) +
        regex_node_strlen(settings->out_dirpatterns) +
        string_node_strlen(settings->out_extensions) +
        regex_node_strlen(settings->out_filepatterns) +
        filetype_node_strlen(settings->out_filetypes) +
        string_node_strlen(settings->paths);
}

size_t settings_strlen(FindSettings *settings)
{
    return strlen(SETTINGS_TEMPLATE)
        - (SETTINGS_TOTAL_FIELD_COUNT * 2)
        + SETTINGS_BOOL_FIELD_COUNT
        + all_strings_strlen(settings);
}

void settings_to_string(FindSettings *settings, char *s)
{
    // assumes s has correct allocation size
    char *in_archiveextensions_s = malloc(string_node_strlen(settings->in_archiveextensions) + 1);
    in_archiveextensions_s[0] = '\0';
    string_node_to_string(settings->in_archiveextensions, in_archiveextensions_s);
    char *in_archivefilepatterns_s = malloc(regex_node_strlen(settings->in_archivefilepatterns) + 1);
    in_archivefilepatterns_s[0] = '\0';
    regex_node_to_string(settings->in_archivefilepatterns, in_archivefilepatterns_s);
    char *in_dirpatterns_s = malloc(regex_node_strlen(settings->in_dirpatterns) + 1);
    in_dirpatterns_s[0] = '\0';
    regex_node_to_string(settings->in_dirpatterns, in_dirpatterns_s);
    char *in_extensions_s = malloc(string_node_strlen(settings->in_extensions) + 1);
    in_extensions_s[0] = '\0';
    string_node_to_string(settings->in_extensions, in_extensions_s);
    char *in_filepatterns_s = malloc(regex_node_strlen(settings->in_filepatterns) + 1);
    in_filepatterns_s[0] = '\0';
    regex_node_to_string(settings->in_filepatterns, in_filepatterns_s);
    char *in_filetypes_s = malloc(filetype_node_strlen(settings->in_filetypes) + 1);
    in_filetypes_s[0] = '\0';
    filetype_node_to_string(settings->in_filetypes, in_filetypes_s);
    char *out_archiveextensions_s = malloc(string_node_strlen(settings->out_archiveextensions) + 1);
    out_archiveextensions_s[0] = '\0';
    string_node_to_string(settings->out_archiveextensions, out_archiveextensions_s);
    char *out_archivefilepatterns_s = malloc(regex_node_strlen(settings->out_archivefilepatterns) + 1);
    out_archivefilepatterns_s[0] = '\0';
    regex_node_to_string(settings->out_archivefilepatterns, out_archivefilepatterns_s);
    char *out_dirpatterns_s = malloc(regex_node_strlen(settings->out_dirpatterns) + 1);
    out_dirpatterns_s[0] = '\0';
    regex_node_to_string(settings->out_dirpatterns, out_dirpatterns_s);
    char *out_extensions_s = malloc(string_node_strlen(settings->out_extensions) + 1);
    out_extensions_s[0] = '\0';
    string_node_to_string(settings->out_extensions, out_extensions_s);
    char *out_filepatterns_s = malloc(regex_node_strlen(settings->out_filepatterns) + 1);
    out_filepatterns_s[0] = '\0';
    regex_node_to_string(settings->out_filepatterns, out_filepatterns_s);
    char *out_filetypes_s = malloc(filetype_node_strlen(settings->out_filetypes) + 1);
    out_filetypes_s[0] = '\0';
    filetype_node_to_string(settings->out_filetypes, out_filetypes_s);
    char *paths_s = malloc(string_node_strlen(settings->paths) + 1);
    paths_s[0] = '\0';
    string_node_to_string(settings->paths, paths_s);

    sprintf(s, SETTINGS_TEMPLATE,
        settings->archivesonly,
        settings->colorize,
        settings->debug,
        settings->excludehidden,
        in_archiveextensions_s,
        in_archivefilepatterns_s,
        in_dirpatterns_s,
        in_extensions_s,
        in_filepatterns_s,
        in_filetypes_s,
        settings->includearchives,
        settings->listdirs,
        settings->listfiles,
        out_archiveextensions_s,
        out_archivefilepatterns_s,
        out_dirpatterns_s,
        out_extensions_s,
        out_filepatterns_s,
        out_filetypes_s,
        paths_s,
        settings->printresults,
        settings->printusage,
        settings->printversion,
        settings->recursive,
        settings->verbose);

    size_t total_len = settings_strlen(settings) + 1;

    s[total_len - 1] = '\0';
    size_t actual_len = strlen(s);

    free(in_archiveextensions_s);
    free(in_archivefilepatterns_s);
    free(in_dirpatterns_s);
    free(in_extensions_s);
    free(in_filepatterns_s);
    free(in_filetypes_s);
    free(out_archiveextensions_s);
    free(out_archivefilepatterns_s);
    free(out_dirpatterns_s);
    free(out_extensions_s);
    free(out_filepatterns_s);
    free(out_filetypes_s);
    free(paths_s);
}

void print_settings(FindSettings *settings)
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
        destroy_string_node(settings->in_archiveextensions);
        destroy_regex_node(settings->in_archivefilepatterns);
        destroy_regex_node(settings->in_dirpatterns);
        destroy_string_node(settings->in_extensions);
        destroy_regex_node(settings->in_filepatterns);
        destroy_string_node(settings->in_filetypes);
        destroy_string_node(settings->out_archiveextensions);
        destroy_regex_node(settings->out_archivefilepatterns);
        destroy_regex_node(settings->out_dirpatterns);
        destroy_string_node(settings->out_extensions);
        destroy_regex_node(settings->out_filepatterns);
        destroy_string_node(settings->out_filetypes);
        destroy_string_node(settings->paths);
        free(settings);
    }
}

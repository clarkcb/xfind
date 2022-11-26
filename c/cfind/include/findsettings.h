#ifndef FINDSETTINGS_H
#define FINDSETTINGS_H

#include "intnode.h"
#include "regexnode.h"
#include "stringnode.h"

typedef enum {
    FILEPATH = 0,
    FILENAME = 1,
    FILETYPE = 2
} SortBy;

typedef struct FindSettings {
    unsigned short archivesonly : 1;
    unsigned short debug : 1;
    unsigned short excludehidden : 1;
    StringNode *in_archiveextensions;
    RegexNode *in_archivefilepatterns;
    RegexNode *in_dirpatterns;
    StringNode *in_extensions;
    RegexNode *in_filepatterns;
    IntNode *in_filetypes;
    unsigned short includearchives : 1;
    unsigned short listdirs : 1;
    unsigned short listfiles : 1;
    StringNode *out_archiveextensions;
    RegexNode *out_archivefilepatterns;
    RegexNode *out_dirpatterns;
    StringNode *out_extensions;
    RegexNode *out_filepatterns;
    IntNode *out_filetypes;
    StringNode *paths;
    unsigned short printresults : 1;
    unsigned short printusage : 1;
    unsigned short printversion : 1;
    unsigned short recursive : 1;
    SortBy sortby;
    unsigned short sort_descending : 1;
    unsigned short verbose : 1;
} FindSettings;

FindSettings *default_settings(void);

size_t settings_strlen(FindSettings *settings);

void settings_to_string(FindSettings *settings, char *s);

void print_settings(FindSettings *settings);

void destroy_settings(FindSettings *settings);

SortBy sortby_from_name(const char *name);

void sortby_to_name(const SortBy sortby, char *name);

#endif

#ifndef FINDSETTINGS_H
#define FINDSETTINGS_H

#include "intnode.h"
#include "regexnode.h"
#include "stringnode.h"

typedef struct FindSettings {
    unsigned short archivesonly;
    unsigned short colorize;
    unsigned short debug;
    unsigned short excludehidden;
    StringNode *in_archiveextensions;
    RegexNode *in_archivefilepatterns;
    RegexNode *in_dirpatterns;
    StringNode *in_extensions;
    RegexNode *in_filepatterns;
    IntNode *in_filetypes;
    unsigned short includearchives;
    unsigned short listdirs;
    unsigned short listfiles;
    StringNode *out_archiveextensions;
    RegexNode *out_archivefilepatterns;
    RegexNode *out_dirpatterns;
    StringNode *out_extensions;
    RegexNode *out_filepatterns;
    IntNode *out_filetypes;
    StringNode *paths;
    unsigned short printresults;
    unsigned short printusage;
    unsigned short printversion;
    unsigned short recursive;
    unsigned short verbose;
} FindSettings;

FindSettings *default_settings(void);

size_t settings_strlen(FindSettings *settings);

void settings_to_string(FindSettings *settings, char *s);

void print_settings(FindSettings *settings);

void destroy_settings(FindSettings *settings);

#endif

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
    IN_ARCHIVEEXTENSION    = 0,
    IN_ARCHIVEFILEPATTERN  = 1,
    IN_DIRPATTERN          = 2,
    IN_EXTENSION           = 3,
    IN_FILEPATTERN         = 4,
    IN_FILETYPE            = 5,
    OUT_ARCHIVEEXT         = 6,
    OUT_ARCHIVEFILEPATTERN = 7,
    OUT_DIRPATTERN         = 8,
    OUT_EXTENSION          = 9,
    OUT_FILEPATTERN        = 10,
    OUT_FILETYPE           = 11
} SettingsCollType;

typedef enum {
    ARCHIVESONLY    = 0,
    DEBUG           = 1,
    EXCLUDEARCHIVES = 2,
    EXCLUDEHIDDEN   = 3,
    INCLUDEARCHIVES = 4,
    INCLUDEHIDDEN   = 5,
    HELP            = 6,
    LISTDIRS        = 7,
    LISTFILES       = 8,
    NORECURSIVE     = 9,
    RECURSIVE       = 10,
    VERBOSE         = 11,
    VERSION         = 12
} SettingsFlagType;

FindOption *new_find_option(const char *longarg, const char *shortarg, const char *desc);

FindOptions *empty_find_options(void);

FindOptions *new_find_options(FindOption *o);

void add_to_find_options(FindOption *o, FindOptions *options);

FindOptions *get_find_options(void);

int settings_from_args(const int argc, char *argv[], FindSettings *settings);

size_t find_options_count(FindOptions *options);

size_t find_option_usage_strlen(FindOption *o, size_t longest_opt_len);

size_t find_options_usage_strlen(FindOptions *options);

void find_options_to_usage_string(FindOptions *options, char *s);

void print_usage(void);

void destroy_find_option(FindOption *o);

void destroy_find_options(FindOptions *options);

#endif

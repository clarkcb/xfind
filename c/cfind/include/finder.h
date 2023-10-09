#ifndef FINDER_H
#define FINDER_H

#include <magic.h>

#include "fileresults.h"
#include "filetypes.h"
#include "findsettings.h"

typedef struct Finder {
    FindSettings *settings;
    FileTypes *file_types;
    magic_t magic_cookie;
} Finder;

Finder *new_finder(const FindSettings *s, const FileTypes *ft, const magic_t magic_cookie);

error_t validate_settings(const FindSettings *settings);

unsigned short is_matching_dir(const char *dir, const FindSettings *settings);

unsigned short has_matching_wildcard_mime_type(const char *mime_type, const StringNode *mime_types);

unsigned short is_matching_mime_type(const char *mime_type, const FindSettings *settings);

unsigned short is_matching_file(const char *dir, const char *file_name, const Finder *finder, FileType *file_type,
                                char *mime_type, struct stat *fpstat);

unsigned short filter_file(const char *dir, const char *file_name, const Finder *finder, FileType *file_type,
                           char *mime_type, struct stat *fpstat);

error_t find(const FindSettings *settings, FileResults *results);

void destroy_finder(Finder *finder);

#endif

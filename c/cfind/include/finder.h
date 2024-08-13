#ifndef FINDER_H
#define FINDER_H

#include "fileresults.h"
#include "filetypes.h"
#include "findsettings.h"

typedef struct Finder {
    FindSettings *settings;
    FileTypes *file_types;
} Finder;

Finder *new_finder(const FindSettings *s, const FileTypes *ft);

error_t validate_settings(const FindSettings *settings);

unsigned short is_matching_dir(const FindSettings *settings, const char *dir);

unsigned short is_matching_file(const FindSettings *settings, const char *file_name,
                                const FileType *file_type, const struct stat *fpstat);

unsigned short filter_file(const FindSettings *settings, const char *dir, const char *file_name,
                           const FileType *file_type, const struct stat *fpstat);

error_t find(const FindSettings *settings, FileResults *results);

void destroy_finder(Finder *finder);

#endif

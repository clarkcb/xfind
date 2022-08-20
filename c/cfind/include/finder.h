#ifndef FINDER_H
#define FINDER_H

#include "fileresults.h"
#include "filetypes.h"
#include "findsettings.h"

typedef struct Finder {
    FindSettings *settings;
    FileTypes *filetypes;
} Finder;

Finder *new_finder(const FindSettings *s, const FileTypes *ft);

error_t validate_settings(const FindSettings *settings);

error_t find(const FindSettings *settings, FileResults *results);

void destroy_finder(Finder *finder);

#endif

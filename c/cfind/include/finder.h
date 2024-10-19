#ifndef FINDER_H
#define FINDER_H

#include "fileresults.h"
#include "filetypes.h"
#include "findsettings.h"
#include "pathnode.h"

typedef struct Finder {
    FindSettings *settings;
    FileTypes *file_types;
} Finder;

Finder *new_finder(const FindSettings *s, const FileTypes *ft);

error_t validate_settings(const FindSettings *settings);

unsigned short is_matching_dir(const FindSettings *settings, const char *dir);

unsigned short is_matching_path(const FindSettings *settings, const Path *path,
                                const FileType *file_type, uint64_t file_size, long last_mod);

unsigned short filter_path(const FindSettings *settings, const Path *path,
                           const FileType *file_type, uint64_t file_size, long last_mod);

error_t filter_to_file_results(const Finder *finder, const PathNode *file_paths, FileResults *results);

error_t filter_paths_to_file_results(const Finder *finder, const PathNode *file_paths, FileResults *results);

error_t find(const FindSettings *settings, FileResults *results);

void destroy_finder(Finder *finder);

#endif

#ifndef FINDER_H
#define FINDER_H

#include <stdbool.h>

#include "fileresults.h"
#include "filetypes.h"
#include "findsettings.h"
#include "pathnode.h"

typedef struct Finder {
    FindSettings *settings;
    FileTypes *file_types;
} Finder;

Finder *new_finder(const FindSettings *settings);

error_t validate_settings(const Finder *finder);

bool is_matching_dir_path_by_hidden(const Finder *finder, const char *dir_path);

bool is_matching_dir_path_by_in_patterns(const Finder *finder, const char *dir_path);

bool is_matching_dir_path_by_out_patterns(const Finder *finder, const char *dir_path);

bool is_traversable_dir_path(const Finder *finder, const char *dir_path);

bool is_matching_dir_path(const Finder *finder, const char *dir_path);

bool is_null_or_matching_dir_path(const Finder *finder, const char *dir_path);

bool is_matching_file_name_by_hidden(const Finder *finder, const char *file_name);

bool is_matching_archive_extension(const Finder *finder, const char *ext);

bool has_matching_archive_extension(const Finder *finder, const char *file_name);

bool is_matching_archive_file_name(const Finder *finder, const char *file_name);

bool is_matching_archive_file_path(const Finder *finder, const Path *file_path);

bool is_matching_archive_file_result(const Finder *finder, const FileResult *result);

bool is_matching_extension(const Finder *finder, const char *ext);

bool has_matching_extension(const Finder *finder, const char *file_name);

bool is_matching_file_name(const Finder *finder, const char *file_name);

bool is_matching_file_type(const Finder *finder, const FileType *file_type);

bool is_matching_file_size(const Finder *finder, unsigned long file_size);

bool is_matching_last_mod(const Finder *finder, long last_mod);

bool is_matching_file_path(const Finder *finder, const Path *file_path);

bool is_matching_file_result(const Finder *finder, const FileResult *result);

error_t filter_file_path_to_file_results(const Finder *finder, const Path *file_path, FileResults *results);

error_t filter_file_paths_to_file_results(const Finder *finder, const PathNode *file_paths, FileResults *results);

error_t find(const Finder *finder, FileResults *results);

void destroy_finder(Finder *finder);

#endif

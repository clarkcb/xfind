#ifndef FILERESULTS_H
#define FILERESULTS_H

#include <stdbool.h>
#include <stdint.h>

#include "filetypes.h"
#include "pathnode.h"
#include "stringnode.h"
#include "findsettings.h"

typedef struct FileResult {
    const Path *path;
    FileType file_type;
    uint64_t file_size;
    long last_mod;
} FileResult;

typedef struct FileResults {
    FileResult *result;
    struct FileResults *next;
} FileResults;

FileResult *new_file_result(const Path *path, FileType file_type, uint64_t file_size, long last_mod);

FileResults *empty_file_results(void);

bool is_null_or_empty_file_results(const FileResults *results);

FileResults *new_file_results(FileResult *r);

void add_to_file_results(FileResult *r, FileResults *results);

size_t file_result_strlen(const FileResult *r);

size_t file_results_count(const FileResults *results);

void file_result_to_string(const FileResult *r, char *s);

void sort_file_result_array(FileResult **arr, size_t n, SortBy sort_by, bool case_insensitive);

void reverse_file_result_array(FileResult *arr[], size_t low, size_t high);

StringNode *dir_results(const FileResults *results);

void print_dir_results(const FileResults *results, const FindSettings *settings);

void print_file_results(const FileResults *results, const FindSettings *settings);

void destroy_file_result(FileResult *r);

void destroy_file_results(FileResults *results);

#endif

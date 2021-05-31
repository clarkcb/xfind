#ifndef FILERESULTS_H
#define FILERESULTS_H

#include "filetypes.h"
#include "stringnode.h"

typedef struct FileResult {
    const char *dir;
    const char *filename;
    FileType filetype;
} FileResult;

typedef struct FileResults {
    FileResult *result;
    struct FileResults *next;
} FileResults;

FileResult *new_file_result(const char *dir, const char *filename, FileType filetype);

FileResults *empty_file_results(void);

int is_null_or_empty_file_results(FileResults *results);

FileResults *new_file_results(FileResult *r);

void add_to_file_results(FileResult *r, FileResults *results);

size_t file_result_strlen(FileResult *r);

size_t file_results_count(FileResults *results);

void file_result_to_string(FileResult *r, char *s);

void print_file_results(FileResults *results);

void sort_file_result_array(FileResult *arr[], size_t n);

StringNode *dir_results(FileResults *results);

void print_dir_results(FileResults *results);

void destroy_file_result(FileResult *r);

void destroy_file_results(FileResults *results);

#endif

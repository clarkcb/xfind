#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "fileresults.h"

FileResult *new_file_result(const Path *path, const FileType file_type, const uint64_t file_size, const long last_mod)
{
    FileResult *r = malloc(sizeof(FileResult));
    assert(r != NULL);
    r->path = path;
    r->file_type = file_type;
    r->file_size = file_size;
    r->last_mod = last_mod;
    return r;
}

FileResults *empty_file_results(void)
{
    FileResults *results = malloc(sizeof(FileResults));
    assert(results != NULL);
    results->result = NULL;
    results->next = NULL;
    return results;
}

int is_null_or_empty_file_results(const FileResults *results)
{
    if (results == NULL || results->result == NULL)
        return 1;
    return 0;
}

FileResults *new_file_results(FileResult *r)
{
    FileResults *results = malloc(sizeof(FileResults));
    assert(results != NULL);
    results->result = r;
    results->next = NULL;
    return results;
}

void add_to_file_results(FileResult *r, FileResults *results)
{
    if (results->result == NULL) {
        results->result = r;
    } else {
        FileResults *temp = results;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        temp->next = new_file_results(r);
    }
}

size_t file_result_strlen(const FileResult *r)
{
    return path_strlen(r->path);
    // Include file_size: + 3 is for the space and parens
    // return strnlen(r->dir, MAX_PATH_LENGTH) + strnlen(r->file_name, MAX_FILENAME_LENGTH) + num_digits_ulong(r->file_size) + 3 + 1;
    // Include mtime: + 3 is for the space and parens
    // return strnlen(r->dir, MAX_PATH_LENGTH) + strnlen(r->file_name, MAX_FILENAME_LENGTH) + num_digits_ulong(r->mtime) + 3 + 1;
}

size_t file_results_count(const FileResults *results)
{
    size_t count = 0;
    FileResults *temp = results;
    while (temp != NULL && temp->result != NULL) {
        count++;
        temp = temp->next;
    }
    return count;
}

void file_result_to_string(const FileResult *r, char *s)
{
    if (r->path != NULL) {
        path_to_string(r->path, s);
        // sprintf(s, "%s/%s (%llu)", r->dir, r->file_name, r->file_size);
        // sprintf(s, "%s/%s (%lu)", r->dir, r->file_name, r->mtime);
        s[path_strlen(r->path)] = '\0';
    }
}

void print_file_results(const FileResults *results, const SortBy sort_by, const unsigned short sort_case_insensitive,
                        const unsigned short sort_descending)
{
    const size_t results_count = file_results_count(results);
    FileResult *results_array[results_count];
    int i = 0;
    FileResults *temp = results;
    while (temp != NULL && temp->result != NULL) {
        results_array[i++] = temp->result;
        temp = temp->next;
    }

    if (results_count > 0) {
        if (results_count > 1) {
            sort_file_result_array(results_array, results_count, sort_by, sort_case_insensitive);
        }

        if (sort_descending > 0) {
            reverse_file_result_array(results_array, 0, results_count - 1);
        }

        printf("\nMatching files (%zu):\n", results_count);

        for (i = 0; i < results_count; i++) {
            const size_t frlen = file_result_strlen(results_array[i]);
            char resstr[frlen + 1];
            resstr[0] = '\0';
            file_result_to_string(results_array[i], resstr);
            resstr[frlen] = '\0';
            log_msg(resstr);
        }
    } else {
        printf("\nMatching files: 0\n");
    }
}

// -----------------------------------------------------------------------------
// Case-sensitive comparisons
// -----------------------------------------------------------------------------
// comparator function for file result paths
static int cmp_file_results_by_path(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    return path_cmp((*r1)->path, (*r2)->path);
}

// comparator function for file result filenames
static int cmp_file_results_by_name(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    return path_file_name_cmp((*r1)->path, (*r2)->path);
}

// comparator function for file result sizes
static int cmp_file_results_by_size(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    const int sizecmp = ((int) ((*r1)->file_size - (*r2)->file_size));
    if (sizecmp == 0) {
        return cmp_file_results_by_path(a, b);
    }
    return sizecmp;
}

// comparator function for file result types
static int cmp_file_results_by_type(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    const int typecmp = ((int) ((*r1)->file_type - (*r2)->file_type));
    if (typecmp == 0) {
        return cmp_file_results_by_path(a, b);
    }
    return typecmp;
}

// comparator function for file result lastmod
static int cmp_file_results_by_lastmod(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    const int timecmp = ((int) ((*r1)->last_mod - (*r2)->last_mod));
    if (timecmp == 0) {
        return cmp_file_results_by_path(a, b);
    }
    return timecmp;
}

// -----------------------------------------------------------------------------
// Case-insensitive comparisons
// -----------------------------------------------------------------------------
// comparator function for file result paths
static int cmp_file_results_by_path_ci(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    return path_case_cmp((*r1)->path, (*r2)->path);
}

// comparator function for file result filenames
static int cmp_file_results_by_name_ci(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    return path_file_name_case_cmp((*r1)->path, (*r2)->path);
}

// comparator function for file result sizes
static int cmp_file_results_by_size_ci(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    const int sizecmp = ((int) ((*r1)->file_size - (*r2)->file_size));
    if (sizecmp == 0) {
        return cmp_file_results_by_path_ci(a, b);
    }
    return sizecmp;
}

// comparator function for file result types
static int cmp_file_results_by_type_ci(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    const int typecmp = ((int) ((*r1)->file_type - (*r2)->file_type));
    if (typecmp == 0) {
        return cmp_file_results_by_path_ci(a, b);
    }
    return typecmp;
}

// comparator function for file result lastmod
static int cmp_file_results_by_lastmod_ci(const void *a, const void *b)
{
    const FileResult **r1 = (FileResult **)a;
    const FileResult **r2 = (FileResult **)b;
    const int timecmp = ((int) ((*r1)->last_mod - (*r2)->last_mod));
    if (timecmp == 0) {
        return cmp_file_results_by_path_ci(a, b);
    }
    return timecmp;
}

// sort a FileResult array
void sort_file_result_array(FileResult **arr, const size_t n, const SortBy sort_by, const unsigned short case_insensitive)
{
    if (case_insensitive == 1) {
        switch (sort_by) {
            case FILENAME:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_name_ci);
                break;
            case FILESIZE:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_size_ci);
                break;
            case FILETYPE:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_type_ci);
                break;
            case LASTMOD:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_lastmod_ci);
                break;
            default:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_path_ci);
                break;
        }
    } else {
        switch (sort_by) {
            case FILENAME:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_name);
                break;
            case FILESIZE:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_size);
                break;
            case FILETYPE:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_type);
                break;
            case LASTMOD:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_lastmod);
                break;
            default:
                qsort(arr, n, sizeof(FileResult *), cmp_file_results_by_path);
                break;
        }
    }
}

void reverse_file_result_array(FileResult *arr[], const size_t low, const size_t high) {
    if (low < high)
    {
        FileResult *temp = arr[low];
        arr[low] = arr[high];
        arr[high] = temp;

        reverse_file_result_array(arr, low + 1, high - 1);
    }
}

StringNode *dir_results(const FileResults *results)
{
    StringNode *dir_node = empty_string_node();

    FileResults *temp = results;
    while (temp != NULL && temp->result != NULL && temp->result->path != NULL && temp->result->path->dir != NULL) {
        if (string_matches_string_node(temp->result->path->dir, dir_node) == 0) {
            add_string_to_string_node(temp->result->path->dir, dir_node);
        }
        temp = temp->next;
    }

    return dir_node;
}

// comparator function for strings
static int cmp_strings(const void *a, const void *b)
{
    const char **s1 = (char **)a;
    const char **s2 = (char **)b;
    return strcmp(*s1, *s2);
}

void print_dir_results(const FileResults *results)
{
    StringNode *dir_node = dir_results(results);
    const size_t dir_count = string_node_count(dir_node);
    char *dir_array[dir_count];

    int i = 0;
    StringNode *temp = dir_node;
    while (temp != NULL) {
        dir_array[i++] = (char *)temp->string;
        temp = temp->next;
    }

    qsort(dir_array, dir_count, sizeof(char *), cmp_strings);

    printf("\nMatching directories (%zu):\n", dir_count);

    for (i = 0; i < dir_count; i++) {
        size_t dlen = strnlen(dir_array[i], MAX_PATH_LENGTH);
        char dstr[dlen + 1];
        strncpy(dstr, dir_array[i], dlen);
        dstr[dlen] = '\0';
        log_msg(dstr);
    }
}

void destroy_file_result(FileResult *r)
{
    if (r != NULL) {
        destroy_path(r->path);
        free(r);
    }
}

void destroy_file_results(FileResults *results)
{
    FileResults *current = results;
    while (current != NULL) {
        FileResults *next = current->next;
        destroy_file_result(current->result);
        free(current);
        current = next;
    }
}

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "fileresults.h"
#include "stringnode.h"

FileResult *new_file_result(const char *d, const char *fn, FileType ft)
{
    FileResult *r = malloc(sizeof(FileResult));
    assert(r != NULL);
    r->dir = d;
    r->filename = fn;
    r->filetype = ft;
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

int is_null_or_empty_file_results(FileResults *results)
{
    if (results == NULL || (results->result == NULL && results->next == NULL))
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

size_t file_result_strlen(FileResult *r)
{
    return strlen(r->dir) + strlen(r->filename) + 1;
}

size_t file_results_count(FileResults *results)
{
    size_t count = 0;
    FileResults *temp = results;
    while (temp != NULL) {
        count++;
        temp = temp->next;
    }
    return count;
}

void file_result_to_string(FileResult *r, char *s)
{
    sprintf(s, "%s/%s", r->dir, r->filename);
    s[file_result_strlen(r)] = '\0';
}

void print_file_results(FileResults *results)
{
    size_t results_count = file_results_count(results);
    FileResult *results_array[results_count];
    int i = 0;
    FileResults *temp = results;
    while (temp != NULL) {
        results_array[i++] = temp->result;
        temp = temp->next;
    }

    if (results_count > 1) {
        sort_file_result_array(results_array, results_count);
    }

    printf("\nMatching files (%zu):\n", results_count);

    for (i = 0; i < results_count; i++) {
        size_t frlen = file_result_strlen(results_array[i]);
        char resstr[frlen + 1];
        file_result_to_string(results_array[i], resstr);
        resstr[frlen] = '\0';
        log_msg(resstr);
    }
}

// Define comparator function as per the requirement
static int cmp_file_results(const void *a, const void *b)
{
    FileResult **r1 = (FileResult **)a;
    FileResult **r2 = (FileResult **)b;

    int dircmp = strcmp((*r1)->dir, (*r2)->dir);
    if (dircmp == 0) {
        return strcmp((*r1)->filename, (*r2)->filename);
    }
  
    return dircmp;
}

// sort a FileResult array
void sort_file_result_array(FileResult **arr, size_t n)
{
    qsort(arr, n, sizeof(FileResult*), cmp_file_results);
}

StringNode *dir_results(FileResults *results)
{
    StringNode *dir_node = empty_string_node();

    FileResults *temp = results;
    while (temp != NULL) {
        if (string_matches_string_node(temp->result->dir, dir_node) == 0) {
            add_string_to_string_node(temp->result->dir, dir_node);
        }
        temp = temp->next;
    }

    return dir_node;
}

// comparator function for strings
static int cmp_strings(const void *a, const void *b)
{
    char **s1 = (char **)a;
    char **s2 = (char **)b;
    return strcmp((*s1), (*s2));
}

void print_dir_results(FileResults *results)
{
    StringNode *dir_node = dir_results(results);
    size_t dir_count = string_node_count(dir_node);
    char *dir_array[dir_count];

    int i = 0;
    StringNode *temp = dir_node;
    while (temp != NULL) {
        dir_array[i++] = temp->string;
        temp = temp->next;
    }

    qsort(dir_array, dir_count, sizeof(char *), cmp_strings);

    printf("\nMatching directories (%zu):\n", dir_count);

    for (i = 0; i < dir_count; i++) {
        size_t dlen = strlen(dir_array[i]);
        char dstr[dlen + 1];
        strncpy(dstr, dir_array[i], dlen);
        dstr[dlen] = '\0';
        log_msg(dstr);
    }
}

void destroy_file_result(FileResult *r)
{
    r->dir = NULL;
    r->filename = NULL;
    free(r);
}

void destroy_file_results(FileResults *results)
{
    FileResults *current = results;
    FileResults *next;
    while (current != NULL) {
        next = current->next;
        destroy_file_result(current->result);
        free(current);
        current = next;
    }
}

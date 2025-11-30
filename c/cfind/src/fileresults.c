#include <assert.h>
#include <regex.h>
#include <stdio.h>
#include <string.h>

#include "color.h"
#include "consolecolor.h"
#include "common.h"
#include "fileresults.h"
#include "fileutil.h"

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

bool is_null_or_empty_file_results(const FileResults *results)
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
void sort_file_result_array(FileResult **arr, const size_t n, const SortBy sort_by, const bool case_insensitive)
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

void colorize_string(const char *s, const size_t start_idx, const size_t end_idx, const Color color, char *colorized) {
    if (start_idx > 0) {
        size_t prefix_len = start_idx;
        char prefix[prefix_len + 1];
        prefix[0] = '\0';
        strncpy(prefix, s, prefix_len);
        prefix[prefix_len] = '\0';
        strcat(colorized, prefix);
    }

    size_t match_len = end_idx - start_idx;
    char match[match_len + 1];
    match[0] = '\0';
    strncpy(match, s + start_idx, match_len);
    match[match_len] = '\0';
    char console_color[8];
    console_color[0] = '\0';
    color_to_console_color(color, console_color);
    console_color[7] = '\0';
    strcat(colorized, console_color);
    strcat(colorized, match);
    strcat(colorized, CONSOLE_COLOR_RESET);

    const size_t s_len = strnlen(s, 1024);
    // if (end_idx > 0) {
    if (end_idx < s_len) {
        size_t suffix_len = s_len - end_idx;
        char suffix[suffix_len + 1];
        suffix[0] = '\0';
        strncpy(suffix, s + end_idx, suffix_len);
        suffix[suffix_len] = '\0';
        strcat(colorized, suffix);
    }
    colorized[s_len + CONSOLE_COLOR_LENGTH + 1] = '\0';
}

void format_dir(const char *dir, const FindSettings *settings, char *formatted) {
    const size_t dir_len = strnlen(dir, 1024);

    size_t nmatch = 1;
    regmatch_t pmatch[nmatch];

    if (settings->colorize
        && string_matches_regex_node_with_matches(dir, settings->in_dir_patterns, nmatch, pmatch) > 0) {
        colorize_string(dir, pmatch[0].rm_so, pmatch[0].rm_eo, settings->dir_color, formatted);
    } else {
        // no colorization
        strncpy(formatted, dir, dir_len);
        formatted[dir_len] = '\0';
    }
}

void format_file_name(const char *file_name, const FindSettings *settings, char *formatted) {
    const size_t file_name_len = strnlen(file_name, 1024);

    size_t nmatch = 1;
    regmatch_t pmatch[nmatch];

    if (settings->colorize && (is_null_or_empty_string_node(settings->in_extensions) == 0
        || is_null_or_empty_regex_node(settings->in_file_patterns) == 0)) {
        char name_only[file_name_len + 1];
        name_only[0] = '\0';
        get_file_name_without_extension(file_name, name_only);
        size_t name_only_len = strnlen(name_only, file_name_len);
        char ext[file_name_len + 1];
        ext[0] = '\0';
        get_extension(file_name, ext);
        size_t ext_len = strnlen(ext, file_name_len);

        if (string_matches_regex_node_with_matches(name_only, settings->in_file_patterns, nmatch, pmatch) > 0) {
            char colorized_name_only[file_name_len + CONSOLE_COLOR_LENGTH + 1];
            colorized_name_only[0] = '\0';
            colorize_string(name_only, pmatch[0].rm_so, pmatch[0].rm_eo, settings->file_color, colorized_name_only);
            name_only_len += CONSOLE_COLOR_LENGTH;
            colorized_name_only[name_only_len] = '\0';
            strncat(formatted, colorized_name_only, name_only_len);
        } else {
            strncat(formatted, name_only, name_only_len);
        }
        strncat(formatted, ".", 1);
        if (is_null_or_empty_string_node(settings->in_extensions) == 0) {
            char colorized_ext[file_name_len + CONSOLE_COLOR_LENGTH + 1];
            colorized_ext[0] = '\0';
            colorize_string(ext, 0, ext_len, settings->ext_color, colorized_ext);
            ext_len += CONSOLE_COLOR_LENGTH;
            colorized_ext[ext_len] = '\0';
            strncat(formatted, colorized_ext, ext_len);
        } else {
            strncat(formatted, ext, ext_len);
        }
        formatted[name_only_len + ext_len + 1] = '\0';
    } else {
        // no colorization
        strncpy(formatted, file_name, file_name_len);
        formatted[file_name_len] = '\0';
    }
}

void format_file_result(const FileResult *fr, const FindSettings *settings, char *formatted)
{
    const size_t path_len = path_strlen(fr->path);

    // dir
    const size_t dir_len = strnlen(fr->path->dir, path_len);

    // colorized dir_len
    size_t cdir_len = dir_len;
    if (settings->colorize && is_null_or_empty_regex_node(settings->in_dir_patterns) == 0) {
        cdir_len += CONSOLE_COLOR_LENGTH;
    }

    char dir_str[cdir_len + 1];
    dir_str[0] = '\0';

    format_dir(fr->path->dir, settings, dir_str);
    dir_str[cdir_len + 1] = '\0';

    // file_name
    const size_t file_name_len = strnlen(fr->path->file_name, path_len);

    // colorized file_name_len
    size_t cfile_name_len = file_name_len;
    if (settings->colorize) {
        if (is_null_or_empty_string_node(settings->in_extensions) == 0) {
            cfile_name_len += CONSOLE_COLOR_LENGTH;
        }
        if (is_null_or_empty_regex_node(settings->in_file_patterns) == 0) {
            cfile_name_len += CONSOLE_COLOR_LENGTH;
        }
    }

    char file_name_str[cfile_name_len + 1];
    file_name_str[0] = '\0';

    format_file_name(fr->path->file_name, settings, file_name_str);

    join_path(dir_str, file_name_str, formatted);
}

void print_dir_results(const FileResults *results, const FindSettings *settings)
{
    StringNode *dir_node = dir_results(results);
    const size_t dir_count = string_node_count(dir_node);

    if (dir_count > 0) {
        char *dir_array[dir_count];

        int i = 0;
        StringNode *temp = dir_node;
        while (temp != NULL) {
            dir_array[i++] = (char *)temp->string;
            temp = temp->next;
        }

        if (dir_count > 1) {
            qsort(dir_array, dir_count, sizeof(char *), cmp_strings);

            if (settings->sort_descending > 0) {
                // TODO: reverse the array
            }
        }

        printf("\nMatching directories (%zu):\n", dir_count);

        if (settings->colorize && is_null_or_empty_regex_node(settings->in_dir_patterns) == 0) {
            for (i = 0; i < dir_count; i++) {
                size_t dlen = strnlen(dir_array[i], MAX_PATH_LENGTH);
                char dstr[dlen + CONSOLE_COLOR_LENGTH + 1];
                dstr[0] = '\0';
                format_dir(dir_array[i], settings, dstr);
                dstr[dlen + CONSOLE_COLOR_LENGTH + 1] = '\0';
                log_msg(dstr);
            }

        } else {
            for (i = 0; i < dir_count; i++) {
                size_t dlen = strnlen(dir_array[i], MAX_PATH_LENGTH);
                char dstr[dlen + 1];
                strncpy(dstr, dir_array[i], dlen);
                dstr[dlen] = '\0';
                log_msg(dstr);
            }
        }

    } else {
        printf("\nMatching directories: 0\n");
    }

    destroy_string_node(dir_node);
}

void print_file_results(const FileResults *results, const FindSettings *settings)
{
    if (settings->debug) {
        printf("DEBUG: print_file_results\n");
    }
    const size_t results_count = file_results_count(results);

    if (results_count > 0) {
        FileResult *results_array[results_count];

        int i = 0;
        FileResults *temp = results;
        while (temp != NULL && temp->result != NULL) {
            results_array[i++] = temp->result;
            temp = temp->next;
        }

        if (results_count > 1) {
            sort_file_result_array(results_array, results_count, settings->sort_by, settings->sort_case_insensitive);

            if (settings->sort_descending > 0) {
                reverse_file_result_array(results_array, 0, results_count - 1);
            }
        }

        printf("\nMatching files (%zu):\n", results_count);
        if (settings->colorize && (is_null_or_empty_regex_node(settings->in_dir_patterns) == 0
            || is_null_or_empty_string_node(settings->in_extensions) == 0
            || is_null_or_empty_regex_node(settings->in_file_patterns) == 0)) {
            for (i = 0; i < results_count; i++) {

                const FileResult *fr = results_array[i];

                const size_t path_len = path_strlen(fr->path);
                const size_t dir_len = strnlen(fr->path->dir, path_len);
                const size_t file_name_len = strnlen(fr->path->file_name, path_len);

                if (settings->debug) {
                    printf("DEBUG: path_len: %zu\n", path_len);
                    printf("DEBUG: dir_len: %zu\n", dir_len);
                    printf("DEBUG: file_name_len: %zu\n", file_name_len);
                }

                // colorized lengths (lengths + 9 for color+reset)
                size_t cdir_len = dir_len;
                if (is_null_or_empty_regex_node(settings->in_dir_patterns) == 0) {
                    cdir_len += 9;
                }
                size_t cfile_name_len = file_name_len;
                if (is_null_or_empty_string_node(settings->in_extensions) == 0) {
                    cfile_name_len += 9;
                }
                if (is_null_or_empty_regex_node(settings->in_file_patterns) == 0) {
                    cfile_name_len += 9;
                }

                if (settings->debug) {
                    printf("DEBUG: cdir_len: %zu\n", cdir_len);
                    printf("DEBUG: cfile_name_len: %zu\n", cfile_name_len);
                }

                char file_path_str[cdir_len + cfile_name_len + 1];
                file_path_str[0] = '\0';

                format_file_result(fr, settings, file_path_str);
                log_msg(file_path_str);
            }
        } else {
            for (i = 0; i < results_count; i++) {
                const size_t frlen = file_result_strlen(results_array[i]);
                char resstr[frlen + 1];
                resstr[0] = '\0';
                file_result_to_string(results_array[i], resstr);
                resstr[frlen] = '\0';
                log_msg(resstr);
            }
        }
    } else {
        printf("\nMatching files: 0\n");
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

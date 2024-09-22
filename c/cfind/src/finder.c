#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "common.h"
#include "finderr.h"
#include "fileresults.h"
#include "fileutil.h"
#include "finder.h"

Finder *new_finder(const FindSettings *s, const FileTypes *ft)
{
    Finder *f = malloc(sizeof(Finder));
    assert(f != NULL);
    f->settings = (FindSettings *)s;
    f->file_types = (FileTypes *)ft;
    return f;
}

error_t validate_settings(const FindSettings *settings)
{
    const size_t path_count = string_node_count(settings->paths);
    if (path_count < 1) {
        return E_STARTPATH_NOT_DEFINED;
    }
    const StringNode *path = settings->paths;
    while (path != NULL) {
        if (!dir_or_file_exists(path->string)) {
            return E_STARTPATH_NOT_FOUND;
        }
        if (access(path->string, R_OK) != 0) {
            return E_STARTPATH_NOT_READABLE;
        }
        path = path->next;
    }
    if (settings->max_depth > -1 && settings->max_depth < settings->min_depth) {
        return E_INVALID_DEPTH_RANGE;
    }
    if (settings->max_last_mod > 0 && settings->max_last_mod < settings->min_last_mod) {
        return E_INVALID_LASTMOD_RANGE;
    }
    if (settings->max_size > 0 && settings->max_size < settings->min_size) {
        return E_INVALID_SIZE_RANGE;
    }
    return E_OK;
}

unsigned short is_matching_dir(const FindSettings *settings, const char *dir)
{
    // null or empty dir is a match
    if (dir == NULL) return 1;
    const size_t dir_len = strnlen(dir, MAX_PATH_LENGTH);
    if (dir_len == 0) return 1;

    // Split into dir elements to match against
    StringNode *dir_elems = new_string_node_from_char_split(PATH_SEPARATOR, dir);
    if (dir_elems == NULL) return 1;

    const StringNode *d = dir_elems;
    unsigned short matches = 1;
    while (matches == 1 && d != NULL && d->string != NULL) {
        if (!strncmp(d->string, ".", 5) || !strncmp(d->string, "..", 5)) {
            d = d->next;
            continue;
        }
        if (!settings->include_hidden && is_hidden(d->string)) {
            matches = 0;
            continue;
        }
        if ((is_null_or_empty_regex_node(settings->in_dir_patterns) == 0
             && string_matches_regex_node(d->string, settings->in_dir_patterns) == 0)
            || (is_null_or_empty_regex_node(settings->out_dir_patterns) == 0
                && string_matches_regex_node(d->string, settings->out_dir_patterns) == 1)) {
            matches = 0;
        }
        d = d->next;
    }
    destroy_string_node(dir_elems);
    return matches;
}

unsigned short is_matching_archive_extension(const FindSettings *settings, const char *ext)
{
    return (is_null_or_empty_string_node(settings->in_archive_extensions) == 1
            || string_matches_string_node(ext, settings->in_archive_extensions) == 1)
        && (is_null_or_empty_string_node(settings->out_archive_extensions) == 1
            || string_matches_string_node(ext, settings->out_archive_extensions) == 0);
}

unsigned short is_matching_extension(const FindSettings *settings, const char *ext)
{
    return (is_null_or_empty_string_node(settings->in_extensions) == 1
            || string_matches_string_node(ext, settings->in_extensions) == 1)
        && (is_null_or_empty_string_node(settings->out_extensions) == 1
            || string_matches_string_node(ext, settings->out_extensions) == 0);
}

unsigned short has_matching_archive_extension(const FindSettings *settings, const char *file_name)
{
    if (is_null_or_empty_string_node(settings->in_archive_extensions) == 1
        && is_null_or_empty_string_node(settings->out_archive_extensions) == 1) {
        return 1;
    }
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    char ext[file_len];
    ext[0] = '\0';
    get_extension(file_name, ext);
    return is_matching_archive_extension(settings, ext);
}

unsigned short has_matching_extension(const FindSettings *settings, const char *file_name)
{
    if (is_null_or_empty_string_node(settings->in_extensions) == 1
        && is_null_or_empty_string_node(settings->out_extensions) == 1) {
        return 1;
    }
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    char ext[file_len];
    ext[0] = '\0';
    get_extension(file_name, ext);
    return is_matching_extension(settings, ext);
}

unsigned short is_matching_archive_file_name(const FindSettings *settings, const char *file_name)
{
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    return (is_null_or_empty_regex_node(settings->in_archive_file_patterns) == 1
            || string_matches_regex_node(file_name, settings->in_archive_file_patterns) == 1)
        && (is_null_or_empty_regex_node(settings->out_archive_file_patterns) == 1
            || string_matches_regex_node(file_name, settings->out_archive_file_patterns) == 0);
}

unsigned short is_matching_file_name(const FindSettings *settings, const char *file_name)
{
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    return (is_null_or_empty_regex_node(settings->in_file_patterns) == 1
            || string_matches_regex_node(file_name, settings->in_file_patterns) == 1)
        && (is_null_or_empty_regex_node(settings->out_file_patterns) == 1
            || string_matches_regex_node(file_name, settings->out_file_patterns) == 0);
}

unsigned short is_matching_file_type(const FindSettings *settings, const FileType *file_type)
{
    return (is_null_or_empty_int_node(settings->in_file_types) == 1
            || int_matches_int_node((int *)file_type, settings->in_file_types) == 1)
        && (is_null_or_empty_int_node(settings->out_file_types) == 1
            || int_matches_int_node((int *)file_type, settings->out_file_types) == 0);
}

unsigned short is_matching_file_size(const FindSettings *settings, const unsigned long file_size)
{
    return (settings->max_size == 0L
            || file_size <= settings->max_size)
        && (settings->min_size == 0L
            || file_size >= settings->min_size);
}

unsigned short is_matching_last_mod(const FindSettings *settings, const long last_mod)
{
    return (settings->max_last_mod == 0L
            || last_mod <= settings->max_last_mod)
        && (settings->min_last_mod == 0L
            || last_mod >= settings->min_last_mod);
}

unsigned short is_matching_file(const FindSettings *settings, const char *file_name,
                                const FileType *file_type, const uint64_t file_size,
                                const long last_mod)
{
    if (*file_type == ARCHIVE) {
        if (settings->include_archives == 0) return 0;
        return has_matching_archive_extension(settings, file_name) == 1
            && is_matching_archive_file_name(settings, file_name) == 1;
    }
    if (settings->archives_only == 1) return 0;
    return has_matching_extension(settings, file_name) == 1
        && is_matching_file_name(settings, file_name) == 1
        && is_matching_file_type(settings, file_type) == 1
        && is_matching_file_size(settings, file_size) == 1
        && is_matching_last_mod(settings, last_mod) == 1;
}

unsigned short filter_file(const FindSettings *settings, const char *dir, const char *file_name,
                           const FileType *file_type, const uint64_t file_size, const long last_mod)
{
    if (settings->include_hidden == 0 && is_hidden(file_name))
        return 0;
    if (is_matching_dir(settings, dir) == 0)
        return 0;
    return is_matching_file(settings, file_name, file_type, file_size, last_mod);
}

// the recursive function
static error_t find_dir(const Finder *finder, const char *dir_path, FileResults *results,
                        const int min_depth, const int max_depth, const int current_depth)
{
    int recurse = 1;
    if (current_depth == max_depth) {
        recurse = 0;
    } else if (max_depth > -1 && current_depth > max_depth) {
        return E_OK;
    }

    DIR *dir = opendir(dir_path);
    if (!dir) {
        if (ENOENT == errno) {
            return E_DIRECTORY_NOT_FOUND;
        }
        return E_UNKNOWN_ERROR;
    }

    size_t dir_len = strnlen(dir_path, MAX_PATH_LENGTH);
    if ((dir_len + 2) >= FILENAME_MAX - 1) {
        return E_FILENAME_TOO_LONG;
    }

    // normalize dir_path (removing trailing / if present)
    char *norm_path = malloc((dir_len + 1) * sizeof(char));
    strncpy(norm_path, dir_path, dir_len);
    norm_path[dir_len] = '\0';
    normalize_path(norm_path);
    const size_t norm_len = strnlen(norm_path, MAX_PATH_LENGTH);

    struct dirent *dent;
    struct stat fpstat;

    // hold directories here to then recurse into after processing this dir
    StringNode *path_dirs = empty_string_node();

    while ((dent = readdir(dir))) {
        if (!strncmp(dent->d_name, ".", 5) || !strncmp(dent->d_name, "..", 5))
            continue;

        char *file_path = malloc((norm_len + strnlen(dent->d_name, MAX_PATH_LENGTH) + 2) * sizeof(char));
        join_path(norm_path, dent->d_name, file_path);

        if (stat(file_path, &fpstat) == -1) {
            // TODO: return err?
            // return errno;
            printf("Can't stat %s\n", file_path);
            continue;
        }

        if (S_ISDIR(fpstat.st_mode)) {
            if (recurse == 1 && is_matching_dir(finder->settings, dent->d_name)) {
                add_string_to_string_node(file_path, path_dirs);
            }
        } else if (S_ISREG(fpstat.st_mode)) {
            FileType file_type = get_file_type(dent->d_name, finder->file_types);
            const uint64_t file_size = fpstat.st_size;
            const long last_mod = fpstat.st_mtime;
            if ((min_depth < 0 || current_depth >= min_depth)
                && filter_file(finder->settings, norm_path, dent->d_name, &file_type, file_size, last_mod) == 1) {
                size_t slen = strnlen(dent->d_name, MAX_PATH_LENGTH);
                char *file_name = malloc((slen + 1) * sizeof(char));
                strncpy(file_name, dent->d_name, slen);
                file_name[slen] = '\0';
                FileResult *r = new_file_result(norm_path, file_name, file_type, fpstat.st_size,
                                                fpstat.st_mtime);
                add_to_file_results(r, results);
            }
        }
    }
    closedir(dir);

    // recursively iterate through path_dirs
    if (is_null_or_empty_string_node(path_dirs) == 0) {
        const StringNode *next_dir = path_dirs;
        while (next_dir != NULL) {
            find_dir(finder, next_dir->string, results, min_depth, max_depth, current_depth + 1);
            next_dir = next_dir->next;
        }
    }

    destroy_string_node(path_dirs);
    return E_OK;
}

static error_t find_path(const Finder *finder, const char *file_path, FileResults *results)
{
    error_t err = E_OK;

    // expand the path in case it has tilde, etc.
    const size_t path_len = strnlen(file_path, MAX_PATH_LENGTH) + 1;
    char *expanded = malloc((path_len + 10) * sizeof(char));
    expanded[0] = '\0';
    expand_path(file_path, &expanded);

    struct stat st;

    if (stat(expanded, &st) == -1) {
        // this shouldn't happen if we made it this far
        err = (error_t)errno;
        if (err == ENOENT) err = E_STARTPATH_NOT_FOUND;
        return err;
    }

    if (S_ISDIR(st.st_mode)) {
        // if max_depth is zero, we can skip since a directory cannot be a result
        if (finder->settings->max_depth == 0) {
            return E_OK;
        }
        if (is_matching_dir(finder->settings, file_path) == 1) {
            const int max_depth = finder->settings->recursive ? finder->settings->max_depth : 1;
            err = find_dir(finder, expanded, results, finder->settings->min_depth,
                max_depth, 1);
            if (err != E_OK) {
                return err;
            }
        } else {
            return E_STARTPATH_NON_MATCHING;
        }
    } else if (S_ISREG(st.st_mode)) {
        // if min_depth > zero, we can skip since the file is at depth zero
        if (finder->settings->min_depth > 0) {
            return E_OK;
        }
        FileType file_type = UNKNOWN;
        const size_t next_path_len = (strnlen(file_path, MAX_PATH_LENGTH) + 2) * sizeof(char);
        char *d = malloc(next_path_len);
        char *f = malloc(next_path_len);
        split_path(file_path, &d, &f);
        if (filter_file(finder->settings, d, f, &file_type, st.st_size, st.st_mtime) == 1) {
            FileResult *r = new_file_result(d, f, file_type, st.st_size,
                                            st.st_mtime);
            add_to_file_results(r, results);
        } else {
            return E_STARTPATH_NON_MATCHING;
        }
    } else {
        return E_STARTPATH_UNSUPPORTED_FILETYPE;
    }

    return err;
}

error_t find(const FindSettings *settings, FileResults *results)
{
    error_t err = validate_settings(settings);
    if (err != E_OK) {
        return err;
    }

    FileTypes *file_types = new_file_types();
    err = get_file_types(file_types);
    if (err != E_OK) {
        return err;
    }

    Finder *finder = new_finder(settings, file_types);

    const StringNode *next_path = settings->paths;
    while (next_path != NULL) {
        // expand the path in case it has tilde, etc.
        // const size_t path_len = strnlen(next_path->string, MAX_PATH_LENGTH) + 1;
        // char *expanded = malloc((path_len + 10) * sizeof(char));
        // expanded[0] = '\0';
        // expand_path(next_path->string, &expanded);

        err = find_path(finder, next_path->string, results);
        if (err != E_OK) {
            destroy_finder(finder);
            return err;
        }

        next_path = next_path->next;
    }

    destroy_finder(finder);
    return err;
}

void destroy_finder(Finder *finder)
{
    // Settings are destroyed separately from finder because they're created first
    // destroy_settings(finder->settings);
    destroy_file_types(finder->file_types);
    free(finder);
}

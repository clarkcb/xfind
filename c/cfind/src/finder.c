#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <libgen.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "common.h"
#include "finderr.h"
#include "fileresults.h"
#include "fileutil.h"
#include "finder.h"

Finder *new_finder(const FindSettings *settings)
{
    Finder *finder = malloc(sizeof(Finder));
    assert(finder != NULL);
    finder->settings = (FindSettings *)settings;
    finder->file_types = get_file_types();

    return finder;
}

static error_t validate_path(const Finder *finder, const Path *path)
{
    // expand the path before checking for existence
    const size_t path_len = path_strlen(path);
    char *path_s = malloc(path_len + 1);
    if (path_s == NULL) return ENOMEM;
    path_s[0] = '\0';
    char *expanded_s = malloc(path_len * 2 + 1);
    if (expanded_s == NULL) return ENOMEM;
    expanded_s[0] = '\0';
    path_to_string(path, path_s);
    expand_path(path_s, &expanded_s);

    Path *expanded_path = new_path(expanded_s);

    error_t err = E_OK;
    if (path_exists(expanded_path)) {
        if (!path_readable(expanded_path)) {
            err = E_STARTPATH_NOT_READABLE;
        }

        struct stat st;

        if (lstat(expanded_s, &st) == -1) {
            // this shouldn't happen if we made it this far
            err = (error_t)errno;
            if (err == ENOENT) err = E_STARTPATH_NOT_FOUND;
        }

        if (err == E_OK) {
            if (S_ISLNK(st.st_mode)) {
                if (finder->settings->follow_symlinks == 0) {
                    err = E_STARTPATH_NON_MATCHING;
                }
            } else if (S_ISDIR(st.st_mode)) {
                if (is_traversable_dir_path(finder, expanded_s) == 0) {
                    err = E_STARTPATH_NON_MATCHING;
                }
            } else if (S_ISREG(st.st_mode)) {
                if (is_matching_file_path(finder, expanded_path) == 0) {
                    err = E_STARTPATH_NON_MATCHING;
                }
            } else {
                err = E_STARTPATH_NON_MATCHING;
            }
        }
    } else {
        err = E_STARTPATH_NOT_FOUND;
    }

    destroy_path(expanded_path);
    free(expanded_s);
    free(path_s);
    return err;
}

error_t validate_settings(const Finder *finder)
{
    const size_t path_count = path_node_count(finder->settings->paths);
    if (path_count < 1) {
        return E_STARTPATH_NOT_DEFINED;
    }

    const PathNode *paths = finder->settings->paths;
    while (paths != NULL && paths->path != NULL) {
        const error_t err = validate_path(finder, paths->path);
        if (err != E_OK) {
            return err;
        }
        paths = paths->next;
    }
    if (finder->settings->max_depth > -1 && finder->settings->max_depth < finder->settings->min_depth) {
        return E_INVALID_DEPTH_RANGE;
    }
    if (finder->settings->max_last_mod > 0 && finder->settings->max_last_mod < finder->settings->min_last_mod) {
        return E_INVALID_LASTMOD_RANGE;
    }
    if (finder->settings->max_size > 0 && finder->settings->max_size < finder->settings->min_size) {
        return E_INVALID_SIZE_RANGE;
    }
    return E_OK;
}

bool is_matching_dir_path_by_hidden(const Finder *finder, const char *dir_path)
{
    return finder->settings->include_hidden || !is_hidden_path(dir_path);
}

bool is_matching_dir_path_by_in_patterns(const Finder *finder, const char *dir_path)
{
    // null or empty dir is a match
    if (dir_path == NULL) return 1;
    const size_t dir_len = strnlen(dir_path, MAX_PATH_LENGTH);
    if (dir_len == 0) return 1;

    // empty in_dir_patterns is a match
    if (is_null_or_empty_regex_node(finder->settings->in_dir_patterns) == 1) {
        return 1;
    }

    // Split into dir elements to match against
    const StringNode *dir_elems = new_string_node_from_char_split(PATH_SEPARATOR, dir_path);
    if (dir_elems == NULL) return 1;

    if (string_node_matches_regex_node(dir_elems, finder->settings->in_dir_patterns) == 1) {
        return 1;
    }
    return 0;
}

bool is_matching_dir_path_by_out_patterns(const Finder *finder, const char *dir_path)
{
    // null or empty dir is a match
    if (dir_path == NULL) return 1;
    const size_t dir_len = strnlen(dir_path, MAX_PATH_LENGTH);
    if (dir_len == 0) return 1;

    // empty out_dir_patterns is a match
    if (is_null_or_empty_regex_node(finder->settings->out_dir_patterns) == 1) {
        return 1;
    }

    // Split into dir elements to match against
    const StringNode *dir_elems = new_string_node_from_char_split(PATH_SEPARATOR, dir_path);
    if (dir_elems == NULL) return 1;

    if (string_node_matches_regex_node(dir_elems, finder->settings->out_dir_patterns) == 1) {
        return 0;
    }
    return 1;
}

bool is_traversable_dir_path(const Finder *finder, const char *dir_path)
{
    return is_matching_dir_path_by_hidden(finder, dir_path)
        && is_matching_dir_path_by_out_patterns(finder, dir_path);
}

bool is_matching_dir_path(const Finder *finder, const char *dir_path)
{
    return is_matching_dir_path_by_hidden(finder, dir_path)
        && is_matching_dir_path_by_in_patterns(finder, dir_path)
        && is_matching_dir_path_by_out_patterns(finder, dir_path);
}

bool is_null_or_matching_dir_path(const Finder *finder, const char *dir_path)
{
    if (dir_path == NULL || strlen(dir_path) == 0) return 1;
    return is_matching_dir_path_by_hidden(finder, dir_path)
        && is_matching_dir_path_by_in_patterns(finder, dir_path)
        && is_matching_dir_path_by_out_patterns(finder, dir_path);
}

bool is_matching_file_name_by_hidden(const Finder *finder, const char *file_name)
{
    return finder->settings->include_hidden || !is_hidden_name(file_name);
}

bool is_matching_archive_extension(const Finder *finder, const char *ext)
{
    return (is_null_or_empty_string_node(finder->settings->in_archive_extensions) == 1
            || string_matches_string_node(ext, finder->settings->in_archive_extensions) == 1)
        && (is_null_or_empty_string_node(finder->settings->out_archive_extensions) == 1
            || string_matches_string_node(ext, finder->settings->out_archive_extensions) == 0);
}

bool has_matching_archive_extension(const Finder *finder, const char *file_name)
{
    if (is_null_or_empty_string_node(finder->settings->in_archive_extensions) == 1
        && is_null_or_empty_string_node(finder->settings->out_archive_extensions) == 1) {
        return 1;
    }
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    char ext[file_len];
    ext[0] = '\0';
    get_extension(file_name, ext);
    return is_matching_archive_extension(finder, ext);
}

bool is_matching_archive_file_name(const Finder *finder, const char *file_name)
{
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    return (is_null_or_empty_regex_node(finder->settings->in_archive_file_patterns) == 1
            || string_matches_regex_node(file_name, finder->settings->in_archive_file_patterns) == 1)
        && (is_null_or_empty_regex_node(finder->settings->out_archive_file_patterns) == 1
            || string_matches_regex_node(file_name, finder->settings->out_archive_file_patterns) == 0);
}

bool is_matching_archive_file_path(const Finder *finder, const Path *file_path)
{
    if (is_null_or_empty_path(file_path) == 1) {
        return 0;
    }

    if (finder->settings->include_archives == 0) return 0;
    return is_null_or_matching_dir_path(finder, file_path->dir) == 1
        && is_matching_file_name_by_hidden(finder, file_path->file_name) == 1
        && has_matching_archive_extension(finder, file_path->file_name) == 1
        && is_matching_archive_file_name(finder, file_path->file_name) == 1;
}

bool is_matching_archive_file_result(const Finder *finder, const FileResult *result)
{
    if (is_null_or_empty_file_result(result) == 1) {
        return 0;
    }

    return is_matching_archive_file_path(finder, result->path) == 1;
}


bool is_matching_extension(const Finder *finder, const char *ext)
{
    return (is_null_or_empty_string_node(finder->settings->in_extensions) == 1
            || string_matches_string_node(ext, finder->settings->in_extensions) == 1)
        && (is_null_or_empty_string_node(finder->settings->out_extensions) == 1
            || string_matches_string_node(ext, finder->settings->out_extensions) == 0);
}

bool has_matching_extension(const Finder *finder, const char *file_name)
{
    if (is_null_or_empty_string_node(finder->settings->in_extensions) == 1
        && is_null_or_empty_string_node(finder->settings->out_extensions) == 1) {
        return 1;
        }
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    char ext[file_len];
    ext[0] = '\0';
    get_extension(file_name, ext);
    return is_matching_extension(finder, ext);
}

bool is_matching_file_name(const Finder *finder, const char *file_name)
{
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    return (is_null_or_empty_regex_node(finder->settings->in_file_patterns) == 1
            || string_matches_regex_node(file_name, finder->settings->in_file_patterns) == 1)
        && (is_null_or_empty_regex_node(finder->settings->out_file_patterns) == 1
            || string_matches_regex_node(file_name, finder->settings->out_file_patterns) == 0);
}

bool is_matching_file_type(const Finder *finder, const FileType *file_type)
{
    return (is_null_or_empty_int_node(finder->settings->in_file_types) == 1
            || int_matches_int_node((int *)file_type, finder->settings->in_file_types) == 1)
        && (is_null_or_empty_int_node(finder->settings->out_file_types) == 1
            || int_matches_int_node((int *)file_type, finder->settings->out_file_types) == 0);
}

bool is_matching_file_size(const Finder *finder, const unsigned long file_size)
{
    return (finder->settings->max_size == 0L
            || file_size <= finder->settings->max_size)
        && (finder->settings->min_size == 0L
            || file_size >= finder->settings->min_size);
}

bool is_matching_last_mod(const Finder *finder, const long last_mod)
{
    return (finder->settings->max_last_mod == 0L
            || last_mod <= finder->settings->max_last_mod)
        && (finder->settings->min_last_mod == 0L
            || last_mod >= finder->settings->min_last_mod);
}

bool is_matching_file_path(const Finder *finder, const Path *file_path)
{
    if (is_null_or_empty_path(file_path) == 1) {
        return 0;
    }

    return is_null_or_matching_dir_path(finder, file_path->dir) == 1
        && is_matching_file_name_by_hidden(finder, file_path->file_name) == 1
        && has_matching_extension(finder, file_path->file_name) == 1
        && is_matching_file_name(finder, file_path->file_name) == 1;
}

bool is_matching_file_result(const Finder *finder, const FileResult *result)
{
    if (is_null_or_empty_file_result(result) == 1) {
        return 0;
    }

    return is_matching_file_path(finder, result->path) == 1
        && is_matching_file_type(finder, &result->file_type) == 1
        && is_matching_file_size(finder, result->file_size) == 1
        && is_matching_last_mod(finder, result->last_mod) == 1;
}

error_t filter_archive_file_path_to_file_results(const Finder *finder, const Path *file_path, FileResults *results)
{
    // Skip if not include_archives and not archives_only
    if (finder->settings->include_archives == 0 && finder->settings->archives_only == 0) {
        return E_STARTPATH_NON_MATCHING;
    }

    if (is_matching_archive_file_path(finder, file_path) == 0) {
        return E_STARTPATH_NON_MATCHING;
    }

    FileResult *r = new_file_result(file_path, ARCHIVE, 0, 0);
    add_to_file_results(r, results);

    return E_OK;
}

error_t filter_reg_file_path_to_file_results(const Finder *finder, const Path *file_path, const FileType file_type,
    FileResults *results)
{
    // Skip if archives_only
    if (finder->settings->archives_only == 1) {
        return E_STARTPATH_NON_MATCHING;
    }

    if (is_matching_file_path(finder, file_path) == 0 || is_matching_file_type(finder, &file_type) == 0) {
        return E_STARTPATH_NON_MATCHING;
    }

    struct stat fpstat;

    if (path_stat(file_path, &fpstat) == -1) {
        // TODO: return err?
        // return errno;
        return E_UNKNOWN_ERROR;
    }

    const uint64_t file_size = fpstat.st_size;
    const long last_mod = fpstat.st_mtime;

    if (is_matching_file_size(finder, file_size) == 0 || is_matching_last_mod(finder, last_mod) == 0) {
        return E_STARTPATH_NON_MATCHING;
    }

    const Path *new_path = copy_path(file_path);
    FileResult *r = new_file_result(new_path, file_type, file_size, last_mod);
    add_to_file_results(r, results);

    return E_OK;
}

error_t filter_file_path_to_file_results(const Finder *finder, const Path *file_path, FileResults *results)
{
    if (is_null_or_empty_path(file_path) == 1) {
        return E_STARTPATH_NON_MATCHING;
    }

    // Skip unless is_null_or_matching_dir_path
    if (!is_null_or_matching_dir_path(finder, file_path->dir)) {
        return E_STARTPATH_NON_MATCHING;
    }

    // Skip hidden unless include_hidden
    if (finder->settings->include_hidden == 0 && is_hidden_name(file_path->file_name)) {
        return E_STARTPATH_NON_MATCHING;
    }

    const FileType file_type = get_file_type(file_path->file_name, finder->file_types);

    if (file_type == ARCHIVE) {
        return filter_archive_file_path_to_file_results(finder, file_path, results);
    }

    return filter_reg_file_path_to_file_results(finder, file_path, file_type, results);
}

error_t filter_file_paths_to_file_results(const Finder *finder, const PathNode *file_paths, FileResults *results)
{
    if (is_null_or_empty_path_node(file_paths) == 1) {
        return E_OK;
    }

    const PathNode *next_path = file_paths;
    while (next_path != NULL && next_path->path != NULL) {
        const Path *path = next_path->path;
        const error_t err = filter_file_path_to_file_results(finder, path, results);
        if (err == E_UNKNOWN_ERROR) {
            return err;
        }

        next_path = next_path->next;
    }
    return E_OK;
}

// the recursive function
static error_t rec_find_path(const Finder *finder, const Path *dir_path, FileResults *results,
                             const int min_depth, const int max_depth, const int current_depth)
{
    error_t err = E_OK;
    int recurse = 1;
    if (current_depth == max_depth) {
        recurse = 0;
    } else if (max_depth > -1 && current_depth > max_depth) {
        return E_OK;
    }

    const size_t dir_path_len = path_strlen(dir_path);

    if ((dir_path_len + 2) >= FILENAME_MAX - 1) {
        return E_FILENAME_TOO_LONG;
    }

    char dir_path_s[dir_path_len + 1];
    dir_path_s[0] = '\0';
    path_to_string(dir_path, dir_path_s);

    DIR *dir = opendir(dir_path_s);
    if (!dir) {
        if (ENOENT == errno) {
            return E_DIRECTORY_NOT_FOUND;
        }
        return E_UNKNOWN_ERROR;
    }

    struct dirent *dent;

    // Get dirs and files under dir_path
    PathNode *path_dirs = empty_path_node();
    PathNode *path_files = empty_path_node();

    while ((dent = readdir(dir))) {
        if (!strncmp(dent->d_name, ".", 5) || !strncmp(dent->d_name, "..", 5))
            continue;

        Path *path = new_path_from_dir_and_file_name(dir_path_s, dent->d_name);
        bool link_to_dir = false;
        bool link_to_file = false;
        if (dent->d_type == DT_LNK) {
            if (finder->settings->follow_symlinks == true) {
                const size_t path_len = path_strlen(path);

                if ((path_len + 2) >= FILENAME_MAX - 1) {
                    return E_FILENAME_TOO_LONG;
                }

                char path_s[path_len + 1];
                path_s[0] = '\0';
                path_to_string(path, path_s);
                char resolved_path[FILENAME_MAX];
                if (realpath(path_s, resolved_path) == NULL) {
                    return E_UNKNOWN_ERROR;
                }

                // Check if it's a directory or regular file
                struct stat st;
                if (stat(resolved_path, &st) == -1) {
                    return E_UNKNOWN_ERROR;
                }

                if (S_ISDIR(st.st_mode)) {
                    link_to_dir = true;
                } else if (S_ISREG(st.st_mode)) {
                    link_to_file = true;
                }

            } else {
                continue;
            }
        }
        if ((dent->d_type == DT_DIR || link_to_dir == true) && recurse == 1
            && is_traversable_dir_path(finder, dent->d_name) == 1) {
            add_path_to_path_node(path, path_dirs);
        } else if ((dent->d_type == DT_REG || link_to_file == true)
            && (min_depth < 0 || current_depth >= min_depth)) {
            add_path_to_path_node(path, path_files);
        }
    }
    closedir(dir);

    // Filter path_files
    filter_file_paths_to_file_results(finder, path_files, results);

    // Recurse path_dirs
    if (is_null_or_empty_path_node(path_dirs) == 0) {
        const PathNode *next_dir = path_dirs;
        while (next_dir != NULL && next_dir->path != NULL) {
            err = rec_find_path(finder, next_dir->path, results, min_depth, max_depth, current_depth + 1);
            if (err != E_OK) break;
            next_dir = next_dir->next;
        }
    }

    destroy_path_node(path_dirs);
    destroy_path_node(path_files);

    return err;
}

static error_t find_path(const Finder *finder, const Path *path, FileResults *results)
{
    error_t err = E_OK;

    // expand the path in case it has tilde, etc.
    const size_t path_len = path_strlen(path) + 1;
    char *path_s = malloc(path_len + 1);
    if (path_s == NULL) return ENOMEM;
    path_s[0] = '\0';
    path_to_string(path, path_s);
    char *expanded = malloc(path_len * 2 + 1);
    if (expanded == NULL) return ENOMEM;
    expanded[0] = '\0';
    expand_path(path_s, &expanded);

    struct stat st;

    // check for symbolic link
    if (lstat(expanded, &st) == -1) {
        // this shouldn't happen if we made it this far
        err = (error_t)errno;
        if (err == ENOENT) err = E_STARTPATH_NOT_FOUND;
    }

    if (err != E_OK) {
        free(expanded);
        free(path_s);
        return err;
    }

    if (S_ISLNK(st.st_mode)) {
        if (finder->settings->follow_symlinks == 1) {
            // call stat to get underlying dir/file
            if (stat(expanded, &st) == -1) {
                // this shouldn't happen if we made it this far
                err = (error_t)errno;
                if (err == ENOENT) err = E_STARTPATH_NOT_FOUND;
            }

            if (err != E_OK) {
                free(expanded);
                free(path_s);
                return err;
            }
        } else {
            free(expanded);
            free(path_s);
            return E_STARTPATH_NON_MATCHING;
        }
    }

    Path *expanded_path = new_path(expanded);

    if (S_ISDIR(st.st_mode)) {
        // if max_depth is zero, we can skip since a directory cannot be a result
        if (finder->settings->max_depth != 0) {
            if (is_traversable_dir_path(finder, expanded) == 1) {
                const int max_depth = finder->settings->recursive ? finder->settings->max_depth : 1;
                err = rec_find_path(finder, expanded_path, results, finder->settings->min_depth,
                    max_depth, 1);
            } else {
                err = E_STARTPATH_NON_MATCHING;
            }
        }

    } else if (S_ISREG(st.st_mode)) {
        // if min_depth > zero, we can skip since the file is at depth zero
        if (finder->settings->min_depth <= 0) {
            err = filter_file_path_to_file_results(finder, expanded_path, results);
        }

    } else {
        err = E_STARTPATH_NON_MATCHING;
    }

    destroy_path(expanded_path);
    free(expanded);
    free(path_s);
    return err;
}

error_t find(const Finder *finder, FileResults *results)
{
    error_t err = validate_settings(finder);
    if (err != E_OK) {
        return err;
    }

    const PathNode *next_path = finder->settings->paths;
    while (next_path != NULL) {
        err = find_path(finder, next_path->path, results);
        if (err != E_OK) {
            break;
        }

        next_path = next_path->next;
    }

    return err;
}

void destroy_finder(Finder *finder)
{
    // Settings are destroyed separately from finder because they're created first
    // destroy_settings(finder->settings);
    destroy_file_types(finder->file_types);
    free(finder);
}

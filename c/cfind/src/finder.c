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
    const size_t path_count = path_node_count(settings->paths);
    if (path_count < 1) {
        return E_STARTPATH_NOT_DEFINED;
    }
    const PathNode *paths = settings->paths;
    while (paths != NULL && paths->path != NULL) {
        if (path_exists(paths->path)) {
            if (!path_readable(paths->path)) {
                return E_STARTPATH_NOT_READABLE;
            }
        } else {
            // expand the path and check again
            const size_t path_len = path_strlen(paths->path);
            char *path_s = malloc(path_len + 1);
            path_s[0] = '\0';
            char *expanded_s = malloc(path_len * 2 + 1);
            expanded_s[0] = '\0';
            path_to_string(paths->path, path_s);
            expand_path(path_s, &expanded_s);

            Path *expanded_path = new_path(expanded_s);

            error_t err = E_OK;
            if (path_exists(expanded_path)) {
                if (!path_readable(expanded_path)) {
                    err = E_STARTPATH_NOT_READABLE;
                }
            } else {
                err = E_STARTPATH_NOT_FOUND;
            }
            destroy_path(expanded_path);
            free(expanded_s);
            if (err != E_OK) {
                return err;
            }
        }
        paths = paths->next;
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

bool is_matching_dir(const FindSettings *settings, const char *dir)
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

bool is_matching_archive_extension(const FindSettings *settings, const char *ext)
{
    return (is_null_or_empty_string_node(settings->in_archive_extensions) == 1
            || string_matches_string_node(ext, settings->in_archive_extensions) == 1)
        && (is_null_or_empty_string_node(settings->out_archive_extensions) == 1
            || string_matches_string_node(ext, settings->out_archive_extensions) == 0);
}

bool is_matching_extension(const FindSettings *settings, const char *ext)
{
    return (is_null_or_empty_string_node(settings->in_extensions) == 1
            || string_matches_string_node(ext, settings->in_extensions) == 1)
        && (is_null_or_empty_string_node(settings->out_extensions) == 1
            || string_matches_string_node(ext, settings->out_extensions) == 0);
}

bool has_matching_archive_extension(const FindSettings *settings, const char *file_name)
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

bool has_matching_extension(const FindSettings *settings, const char *file_name)
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

bool is_matching_archive_file_name(const FindSettings *settings, const char *file_name)
{
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    return (is_null_or_empty_regex_node(settings->in_archive_file_patterns) == 1
            || string_matches_regex_node(file_name, settings->in_archive_file_patterns) == 1)
        && (is_null_or_empty_regex_node(settings->out_archive_file_patterns) == 1
            || string_matches_regex_node(file_name, settings->out_archive_file_patterns) == 0);
}

bool is_matching_file_name(const FindSettings *settings, const char *file_name)
{
    if (file_name == NULL) return 0;
    const size_t file_len = strnlen(file_name, MAX_PATH_LENGTH);
    if (file_len < 1) return 0;
    return (is_null_or_empty_regex_node(settings->in_file_patterns) == 1
            || string_matches_regex_node(file_name, settings->in_file_patterns) == 1)
        && (is_null_or_empty_regex_node(settings->out_file_patterns) == 1
            || string_matches_regex_node(file_name, settings->out_file_patterns) == 0);
}

bool is_matching_file_type(const FindSettings *settings, const FileType *file_type)
{
    return (is_null_or_empty_int_node(settings->in_file_types) == 1
            || int_matches_int_node((int *)file_type, settings->in_file_types) == 1)
        && (is_null_or_empty_int_node(settings->out_file_types) == 1
            || int_matches_int_node((int *)file_type, settings->out_file_types) == 0);
}

bool is_matching_file_size(const FindSettings *settings, const unsigned long file_size)
{
    return (settings->max_size == 0L
            || file_size <= settings->max_size)
        && (settings->min_size == 0L
            || file_size >= settings->min_size);
}

bool is_matching_last_mod(const FindSettings *settings, const long last_mod)
{
    return (settings->max_last_mod == 0L
            || last_mod <= settings->max_last_mod)
        && (settings->min_last_mod == 0L
            || last_mod >= settings->min_last_mod);
}

bool is_matching_path(const FindSettings *settings, const Path *path,
                      const FileType *file_type, const uint64_t file_size,
                      const long last_mod)
{
    if (*file_type == ARCHIVE) {
        if (settings->include_archives == 0) return 0;
        return has_matching_archive_extension(settings, path->file_name) == 1
            && is_matching_archive_file_name(settings, path->file_name) == 1;
    }
    if (settings->archives_only == 1) return 0;
    return has_matching_extension(settings, path->file_name) == 1
        && is_matching_file_name(settings, path->file_name) == 1
        && is_matching_file_type(settings, file_type) == 1
        && is_matching_file_size(settings, file_size) == 1
        && is_matching_last_mod(settings, last_mod) == 1;
}

bool filter_path(const FindSettings *settings, const Path *path, const FileType *file_type,
                 const uint64_t file_size, const long last_mod) {
    if (settings->include_hidden == 0 && is_hidden(path->file_name))
        return 0;
    if (is_matching_dir(settings, path->dir) == 0)
        return 0;
    return is_matching_path(settings, path, file_type, file_size, last_mod);
}

error_t filter_to_file_results(const Finder *finder, const PathNode *file_paths, FileResults *results) {
    if (is_null_or_empty_path_node(file_paths) == 1) {
        return E_OK;
    }

    struct stat fpstat;

    const PathNode *next_path = file_paths;
    while (next_path != NULL && next_path->path != NULL) {
        Path *path = next_path->path;
        if (path_stat(path, &fpstat) == -1) {
            // TODO: return err?
            // return errno;
            return E_UNKNOWN_ERROR;
        }

        // Skip hidden unless include_hidden
        if (finder->settings->include_hidden == 0 && is_hidden_path(path)) {
            next_path = next_path->next;
            continue;
        }

        FileType file_type = get_file_type(path->file_name, finder->file_types);
        const uint64_t file_size = fpstat.st_size;
        const long last_mod = fpstat.st_mtime;

        if (is_matching_path(finder->settings, path, &file_type, file_size, last_mod)) {
            const Path *new_path = copy_path(path);
            FileResult *r = new_file_result(new_path, file_type, file_size, last_mod);
            add_to_file_results(r, results);
        }

        next_path = next_path->next;
    }
    return E_OK;
}

error_t filter_paths_to_file_results(const Finder *finder, const PathNode *file_paths, FileResults *results) {
    // TODO: convert to file paths
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
        if ((dent->d_type == DT_DIR || link_to_dir == true) && recurse == 1 && is_matching_dir(finder->settings, dent->d_name)) {
            add_path_to_path_node(path, path_dirs);
        } else if ((dent->d_type == DT_REG || link_to_file == true) && (min_depth < 0 || current_depth >= min_depth)) {
            add_path_to_path_node(path, path_files);
        }
    }
    closedir(dir);

    // Filter path_files
    filter_to_file_results(finder, path_files, results);

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
    path_s[0] = '\0';
    path_to_string(path, path_s);
    char *expanded = malloc(path_len * 2 + 1);
    expanded[0] = '\0';
    expand_path(path_s, &expanded);

    struct stat st;

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

    Path *expanded_path = new_path(expanded);

    if (S_ISDIR(st.st_mode)) {
        // if max_depth is zero, we can skip since a directory cannot be a result
        if (finder->settings->max_depth == 0) {
            return E_OK;
        }
        if (is_matching_dir(finder->settings, expanded) == 1) {
            const int max_depth = finder->settings->recursive ? finder->settings->max_depth : 1;
            err = rec_find_path(finder, expanded_path, results, finder->settings->min_depth,
                max_depth, 1);
        } else {
            err = E_STARTPATH_NON_MATCHING;
        }

        if (err != E_OK) {
            free(expanded);
            free(path_s);
            return err;
        }

    } else if (S_ISREG(st.st_mode)) {
        // if min_depth > zero, we can skip since the file is at depth zero
        if (finder->settings->min_depth > 0) {
            free(expanded);
            free(path_s);
            return E_OK;
        }
        FileType file_type = UNKNOWN;
        if (filter_path(finder->settings, expanded_path, &file_type, st.st_size, st.st_mtime) == 1) {
            FileResult *r = new_file_result(expanded_path, file_type, st.st_size, st.st_mtime);
            add_to_file_results(r, results);
        } else {
            err = E_STARTPATH_NON_MATCHING;
        }
    } else {
        err = E_STARTPATH_UNSUPPORTED_FILETYPE;
    }

    destroy_path(expanded_path);
    free(expanded);
    free(path_s);
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

    const PathNode *next_path = settings->paths;
    while (next_path != NULL) {
        err = find_path(finder, next_path->path, results);
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

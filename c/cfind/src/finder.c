#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

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
    size_t path_count = string_node_count(settings->paths);
    if (path_count < 1) {
        return E_STARTPATH_NOT_DEFINED;
    }
    StringNode *temp = settings->paths;
    while (temp != NULL) {
        if (!dir_or_file_exists(temp->string)) {
            return E_STARTPATH_NOT_FOUND;
        }
        temp = temp->next;
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

unsigned short is_matching_dir(const char *dir, const FindSettings *settings)
{
    // null or empty dir is a match
    if (dir == NULL) return 1;
    size_t dirlen = strlen(dir);
    if (dirlen == 0) return 1;

    // Split into dir elements to match against
    StringNode *dir_elems = new_string_node_from_char_split(PATH_SEPARATOR, dir);
    if (dir_elems == NULL) return 1;

    StringNode *d = dir_elems;
    unsigned short matches = 1;
    while (matches == 1 && d != NULL && d->string != NULL) {
        if (!settings->include_hidden && is_hidden(d->string)) {
            matches = 0;
        }
        // TODO: right now this matches strings, need to switch to regex
        if (((is_null_or_empty_regex_node(settings->in_dir_patterns) == 0)
            && (string_matches_regex_node(dir, settings->in_dir_patterns) == 0))
            || ((is_null_or_empty_regex_node(settings->out_dir_patterns) == 0)
                && (string_matches_regex_node(dir, settings->out_dir_patterns) == 1))) {
            matches = 0;
        }
        d = d->next;
    }
    destroy_string_node(dir_elems);
    return matches;
}

unsigned short is_matching_file(const char *dir, const char *file_name, const Finder *finder, FileType *file_type, const struct stat *fpstat)
{
    if (file_name == NULL) return 0;
    size_t file_len = strnlen(file_name, 1024);
    if (file_len < 1) return 0;
    unsigned int ext_size;
    int dot_idx = last_index_of_char_in_string('.', file_name);
    if (dot_idx == 0 || dot_idx == file_len - 1) ext_size = 0;
    else ext_size = (unsigned int)file_len - (unsigned int)dot_idx;
    char ext[ext_size + 1];
    if (ext_size > 0) get_extension(file_name, ext);
    else ext[0] = '\0';

    if (*file_type == UNKNOWN) {
        *file_type = get_file_type_for_filename(file_name, finder->file_types);
        if (*file_type == UNKNOWN) {
            *file_type = get_file_type_for_ext(ext, finder->file_types);
        }
    }

    if ((is_null_or_empty_int_node(finder->settings->in_file_types) == 0
          && int_matches_int_node((int *)file_type, finder->settings->in_file_types) == 0)
        || (is_null_or_empty_int_node(finder->settings->out_file_types) == 0
             && int_matches_int_node((int *)file_type, finder->settings->out_file_types) == 1)
        || (finder->settings->max_last_mod > 0L && fpstat->st_mtime > finder->settings->max_last_mod)
        || (finder->settings->min_last_mod > 0L && fpstat->st_mtime < finder->settings->min_last_mod)
        || (finder->settings->max_size > 0L && fpstat->st_size > finder->settings->max_size)
        || (finder->settings->min_size > 0L && fpstat->st_size < finder->settings->min_size)) {
        return 0;
    }

    if (*file_type == ARCHIVE) {
        if (finder->settings->include_archives == 0) return 0;

        if (is_null_or_empty_string_node(finder->settings->in_archive_extensions) == 0
            && string_matches_string_node(ext, finder->settings->in_archive_extensions) == 0
            || is_null_or_empty_string_node(finder->settings->out_archive_extensions) == 0
            && string_matches_string_node(ext, finder->settings->out_archive_extensions) == 1
            || is_null_or_empty_regex_node(finder->settings->in_archive_file_patterns) == 0
            && string_matches_regex_node(file_name, finder->settings->in_archive_file_patterns) == 0
            || is_null_or_empty_regex_node(finder->settings->out_archive_file_patterns) == 0
            && string_matches_regex_node(file_name, finder->settings->out_archive_file_patterns) == 1) {
            return 0;
        }
    } else {
        if (finder->settings->archives_only == 1) return 0;

        if (is_null_or_empty_string_node(finder->settings->in_extensions) == 0
            && string_matches_string_node(ext, finder->settings->in_extensions) == 0
            || is_null_or_empty_string_node(finder->settings->out_extensions) == 0
            && string_matches_string_node(ext, finder->settings->out_extensions) == 1
            || is_null_or_empty_regex_node(finder->settings->in_file_patterns) == 0
            && string_matches_regex_node(file_name, finder->settings->in_file_patterns) == 0
            || is_null_or_empty_regex_node(finder->settings->out_file_patterns) == 0
            && string_matches_regex_node(file_name, finder->settings->out_file_patterns) == 1) {
            return 0;
        }
    }

    return 1;
}

unsigned short filter_file(const char *dir, const char *file_name, const Finder *finder, FileType *file_type, const struct stat *fpstat)
{
    if (!finder->settings->include_hidden && is_hidden(file_name))
        return 0;
    return is_matching_file(dir, file_name, finder, file_type, fpstat);
}

static error_t find_dir(const char *dirpath, const Finder *finder, FileResults *results, const int depth)
{
    DIR *dir = opendir(dirpath);
    if (!dir) {
        destroy_finder((Finder *)finder);
        if (ENOENT == errno) {
            return E_DIRECTORY_NOT_FOUND;
        } else {
            return E_UNKNOWN_ERROR;
        }
    }

    size_t dirlen = strlen(dirpath);
    if ((dirlen + 2) >= FILENAME_MAX - 1) {
        return E_FILENAME_TOO_LONG;
    }

    // normalize dirpath (removing trailing / if present)
    char *normpath = (char *)malloc((dirlen + 1) * sizeof(char));
    strncpy(normpath, dirpath, dirlen);
    normpath[dirlen] = '\0';
    normalize_path(normpath);
    size_t normlen = strlen(normpath);

    struct dirent *dent;
    struct stat fpstat;

    // hold directories here to then recurse into after processing this dir
    StringNode *find_dirs = empty_string_node();

    while ((dent = readdir(dir))) {
        if (!strcmp(dent->d_name, ".") || !strcmp(dent->d_name, ".."))
            continue;

        char *file_path = (char *)malloc((normlen + strlen(dent->d_name) + 2) * sizeof(char));
        join_path(normpath, dent->d_name, file_path);

        if (stat(file_path, &fpstat) == -1) {
            // TODO: return err?
            // return errno;
            printf("Can't stat %s\n", file_path);
            continue;
        }

        if (S_ISDIR(fpstat.st_mode)) {
            if ((finder->settings->max_depth < 1 || depth <= finder->settings->max_depth)
                && finder->settings->recursive
                && is_matching_dir(dent->d_name, finder->settings)) {
                add_string_to_string_node(file_path, find_dirs);
            }
        } else if (S_ISREG(fpstat.st_mode)) {
            FileType file_type = UNKNOWN;
            if (depth >= finder->settings->min_depth
                && (finder->settings->max_depth < 1 || depth <= finder->settings->max_depth)
                && filter_file(normpath, dent->d_name, finder, &file_type, &fpstat) == 1) {
                size_t slen = strlen(dent->d_name);
                char *file_name = (char *)malloc((slen + 1) * sizeof(char));
                strncpy(file_name, dent->d_name, slen);
                file_name[slen] = '\0';
                FileResult *r = new_file_result(normpath, file_name, file_type, fpstat.st_size,
                                                fpstat.st_mtime);
                add_to_file_results(r, results);
            }
        }
    }
    closedir(dir);

    // recursively iterate through find_dirs
    if (is_null_or_empty_string_node(find_dirs) == 0 && finder->settings->recursive) {
        StringNode *temp = find_dirs;
        while (temp != NULL) {
            find_dir(temp->string, finder, results, depth + 1);
            temp = temp->next;
        }
    }

    destroy_string_node(find_dirs);
    return E_OK;
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

    StringNode *nextpath = settings->paths;
    while (nextpath != NULL) {
        // expand the path in case it has tilde, etc.
        size_t path_len = strlen(nextpath->string) + 1;
        char *expanded = (char *)malloc((path_len + 10) * (sizeof(char)));
        expanded[0] = '\0';
        expand_path(nextpath->string, &expanded);

        // check whether the file is a directory or file
        // and route accordingly
        struct stat st;

        if (stat(expanded, &st) == -1) {
            // this shouldn't happen if we made it this far
            err = (error_t)errno;
            destroy_finder(finder);
            if (err == ENOENT) err = E_STARTPATH_NOT_FOUND;
            return err;
        }
        if (S_ISDIR(st.st_mode)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (finder->settings->max_depth != 0) {
                err = find_dir(expanded, finder, results, 1);
            }
            if (err != E_OK) {
                destroy_finder(finder);
                return err;
            }
        } else if (S_ISREG(st.st_mode)) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (finder->settings->min_depth <= 0) {
                FileType file_type = UNKNOWN;
                size_t nextpath_len = (strlen(nextpath->string) + 2) * sizeof(char);
                char *d = (char *)malloc(nextpath_len);
                char *f = (char *)malloc(nextpath_len);
                split_path(nextpath->string, &d, &f);
                if (filter_file(d, f, finder, &file_type, &st) == 1) {
                    FileResult *r = new_file_result(d, f, file_type, (uint64_t) st.st_size,
                                                    st.st_mtime);
                    add_to_file_results(r, results);
                } else {
                    return E_STARTPATH_NON_MATCHING;
                }
            }
        } else {
            return E_STARTPATH_UNSUPPORTED_FILETYPE;
        }
        free(expanded);
        nextpath = nextpath->next;
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

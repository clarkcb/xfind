#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <stdint.h>
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
    f->filetypes = (FileTypes *)ft;
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
    return E_OK;
}

unsigned short is_matching_dir(const char *dir, const FindSettings *settings)
{
    unsigned short res = 0;
    if (((is_null_or_empty_regex_node(settings->in_dirpatterns) == 1)
          || (string_matches_regex_node(dir, settings->in_dirpatterns) != 0))
        && ((is_null_or_empty_regex_node(settings->out_dirpatterns) == 1)
             || (string_matches_regex_node(dir, settings->out_dirpatterns) == 0))) {
        res = 1;
    }
    return res;
}

unsigned short is_matching_file(const char *dir, const char *filename, const Finder *finder, FileType *filetype, struct stat *fpstat)
{
    if (filename == NULL) return 0;
    size_t file_len = strlen(filename);
    if (file_len < 1) return 0;
    unsigned int ext_size;
    int dot_idx = last_index_of_char_in_string('.', filename);
    if (dot_idx == 0 || dot_idx == file_len - 1) ext_size = 0;
    else ext_size = (unsigned int)file_len - (unsigned int)dot_idx;
    char ext[ext_size + 1];
    if (ext_size > 0) get_extension(filename, ext);
    else ext[0] = '\0';

    if (*filetype == UNKNOWN) {
        *filetype = get_filetype_for_filename(filename, finder->filetypes);
        if (*filetype == UNKNOWN) {
            *filetype = get_filetype_for_ext(ext, finder->filetypes);
        }
    }

    if (((is_null_or_empty_int_node(finder->settings->in_filetypes) == 0)
          && (int_matches_int_node((int *)filetype, finder->settings->in_filetypes) == 0))
        || ((is_null_or_empty_int_node(finder->settings->out_filetypes) == 0)
             && (int_matches_int_node((int *)filetype, finder->settings->out_filetypes) == 1))
        || (finder->settings->maxlastmod > 0L && fpstat->st_mtime > finder->settings->maxlastmod)
        || (finder->settings->minlastmod > 0L && fpstat->st_mtime < finder->settings->minlastmod)
        || (finder->settings->maxsize > 0L && fpstat->st_size > finder->settings->maxsize)
        || (finder->settings->minsize > 0L && fpstat->st_size < finder->settings->minsize)) {
        return 0;
    }

    if (*filetype == ARCHIVE) {
        if (finder->settings->includearchives == 0) return 0;

        if (((is_null_or_empty_string_node(finder->settings->in_archiveextensions) == 0)
              && (string_matches_string_node(ext, finder->settings->in_archiveextensions) == 0))
            || ((is_null_or_empty_string_node(finder->settings->out_archiveextensions) == 0)
              && (string_matches_string_node(ext, finder->settings->out_archiveextensions) == 1))
            || ((is_null_or_empty_regex_node(finder->settings->in_archivefilepatterns) == 0)
              && (string_matches_regex_node(filename, finder->settings->in_archivefilepatterns) == 0))
            || ((is_null_or_empty_regex_node(finder->settings->out_archivefilepatterns) == 0)
              && (string_matches_regex_node(filename, finder->settings->out_archivefilepatterns) == 1))) {
            return 0;
        }
    } else {
        if (finder->settings->archivesonly == 1) return 0;

        if (((is_null_or_empty_string_node(finder->settings->in_extensions) == 0)
              && (string_matches_string_node(ext, finder->settings->in_extensions) == 0))
            || ((is_null_or_empty_string_node(finder->settings->out_extensions) == 0)
              && (string_matches_string_node(ext, finder->settings->out_extensions) == 1))
            || ((is_null_or_empty_regex_node(finder->settings->in_filepatterns) == 0)
              && (string_matches_regex_node(filename, finder->settings->in_filepatterns) == 0))
            || ((is_null_or_empty_regex_node(finder->settings->out_filepatterns) == 0)
              && (string_matches_regex_node(filename, finder->settings->out_filepatterns) == 1))) {
            return 0;
        }
    }

    return 1;
}

unsigned short filter_file(const char *dir, const char *filename, const Finder *finder, FileType *filetype, struct stat *fpstat)
{
    if (finder->settings->excludehidden && is_hidden(filename))
        return 0;
    return is_matching_file(dir, filename, finder, filetype, fpstat);
}

static error_t find_dir(const char *dirpath, const Finder *finder, FileResults *results)
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
    StringNode *finddirs = empty_string_node();

    while ((dent = readdir(dir))) {
        if (!strcmp(dent->d_name, ".") || !strcmp(dent->d_name, ".."))
            continue;

        char *filepath = (char *)malloc((normlen + strlen(dent->d_name) + 2) * sizeof(char));
        join_path(normpath, dent->d_name, filepath);

        if (stat(filepath, &fpstat) == -1) {
            // TODO: return err?
            // return errno;
            printf("Can't stat %s\n", filepath);
            continue;
        }

        if (S_ISDIR(fpstat.st_mode)) {
            if (finder->settings->recursive
                && (!finder->settings->excludehidden || !is_hidden(dent->d_name))
                && is_matching_dir(dent->d_name, finder->settings)) {
                add_string_to_string_node(filepath, finddirs);
            }
        } else if (S_ISREG(fpstat.st_mode)) {
            FileType filetype = UNKNOWN;
            if (filter_file(normpath, dent->d_name, finder, &filetype, &fpstat) == 1) {
                size_t slen = strlen(dent->d_name);
                char *filename = (char *)malloc((slen + 1) * sizeof(char));
                strncpy(filename, dent->d_name, slen);
                filename[slen] = '\0';
                // FileResult *r = new_file_result(normpath, filename, filetype, &fpstat);
                FileResult *r = new_file_result(normpath, filename, filetype, (uint64_t) fpstat.st_size,
                                                fpstat.st_mtime);
                add_to_file_results(r, results);
            }
        }
    }
    closedir(dir);

    // recursively iterate through finddirs
    if (is_null_or_empty_string_node(finddirs) == 0 && finder->settings->recursive) {
        StringNode *temp = finddirs;
        while (temp != NULL) {
            find_dir(temp->string, finder, results);
            temp = temp->next;
        }
    }

    destroy_string_node(finddirs);
    return E_OK;
}

error_t find(const FindSettings *settings, FileResults *results)
{
    error_t err = validate_settings(settings);
    if (err != E_OK) {
        return err;
    }

    FileTypes *filetypes = new_filetypes();
    err = get_filetypes(filetypes);
    if (err != E_OK) {
        return err;
    }

    Finder *finder = new_finder(settings, filetypes);

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
            err = errno;
            destroy_finder(finder);
            if (err == ENOENT) err = E_STARTPATH_NOT_FOUND;
            return err;
        }
        if (S_ISDIR(st.st_mode)) {
            err = find_dir(expanded, finder, results);
            if (err != E_OK) {
                destroy_finder(finder);
                return err;
            }
        } else if (S_ISREG(st.st_mode)) {
            FileType filetype = UNKNOWN;
            size_t nextpath_len = (strlen(nextpath->string) + 2) * sizeof(char);
            char *d = (char *)malloc(nextpath_len);
            char *f = (char *)malloc(nextpath_len);
            split_path(nextpath->string, &d, &f);
            if (filter_file(d, f, finder, &filetype, &st) == 1) {
                // FileResult *r = new_file_result(d, f, filetype, &st);
                FileResult *r = new_file_result(d, f, filetype, (uint64_t) st.st_size,
                                                st.st_mtime);
                add_to_file_results(r, results);
            } else {
                return E_STARTPATH_NON_MATCHING;
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
    destroy_filetypes(finder->filetypes);
    free(finder);
}

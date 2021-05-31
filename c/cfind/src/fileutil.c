#include <assert.h>
#include <errno.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "common.h"
#include "fileutil.h"

unsigned short dir_or_file_exists(const char *filepath)
{
    struct stat statbuf;
    if (stat(filepath, &statbuf) == -1) {
        if (ENOENT == errno) {
            return 0; // does not exist
        }
    }
    return 1;
}

void get_extension(const char *filename, char *ext)
{
    int idx = last_index_of_char_in_string('.', filename);
    size_t slen = strlen(filename);
    if (idx < 1 || (idx == 1 && filename[0] == '.') || idx >= (slen - 1)) {
        ext = "";
    } else {
        size_t ext_size = slen - (size_t)idx;
        ext = (char *)realloc(ext, ext_size);
        int c = 0;
        for (int i = idx+1; i < slen; i++) {
            ext[c++] = filename[i];
        }
    }
}

unsigned short is_hidden(const char *filepath)
{
    int idx = last_index_of_char_in_string(PATH_SEPARATOR, filepath);
    size_t fplen = strlen(filepath);
    if (idx == -1) {
        idx = 0;
    } else {
        idx++;
    }
    if (idx < fplen && filepath[idx] == '.') {
        // it might be hidden
        if (fplen - (size_t)idx == 1)
            return 0;
        if (fplen - (size_t)idx == 2 && filepath[idx+1] == '.')
            return 0;
        return 1;
    }
    return 0;
}

void join_path(const char *p1, const char *p2, char *joined)
{
    size_t joinedlen = (p1 ? strlen(p1) : 0) +
                       (p2 ? strlen(p2) : 0) + 2;

    if (p1) {
        size_t p1_len = strlen(p1);
        strncpy(joined, p1, p1_len);
        if (p2) {
            joined[p1_len] = PATH_SEPARATOR;
            joined[p1_len + 1] = '\0';
        }
    } else {
        *joined = 0;
    }
    if (p2) {
        strncpy(joined + strlen(joined), p2, strlen(p2));
    }
    joined[joinedlen - 1] = '\0';
}

void split_path(const char *fp, char** p, char** f)
{
    char* slptr = strrchr(fp, PATH_SEPARATOR);
    if (slptr) {
        *p = strndup(fp, (size_t)(slptr - fp));
        *f = strdup(++slptr);
    } else {
        *p = strdup(".");
        *f = strdup(fp);
    }
}

void normalize_path(char *fp)
{
    size_t end_pos = strlen(fp) -1;
    while (fp[end_pos] == '\0') {
        end_pos--;
    }
    if (fp[end_pos] == PATH_SEPARATOR) {
        fp[end_pos] = '\0';
    }
}

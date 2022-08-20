#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <wordexp.h>

#include "common.h"
#include "fileutil.h"

const char *DOT_DIRS[] = {".", "..", "./", "../"};

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

unsigned short is_dot_dir(const char *filepath)
{
    if (filepath == NULL || strlen(filepath) < 1) return 0;
    return index_of_string_in_array(filepath, (char **) DOT_DIRS, 4) > -1;
}

long file_size(const char *filepath)
{
    struct stat st;
    if (stat(filepath, &st) == -1) {
        if (ENOENT == errno) {
            return 0; // does not exist
        }
    }
    return (long) st.st_size;
}

/*
 * get_extension - get a filename's extension, if defined, otherwise an empty string
 *
 * NOTE: ext is expected to have enough allocated to contain the extension; the
 *       easy way to ensure that is to alloc to the size of the filename
 */
void get_extension(const char *filename, char *ext)
{
    if (filename == NULL) return;
    size_t fnlen = strlen(filename);
    // b.c is the shortest a filename can be with ext, so skip if shorter than 3
    if (fnlen < 3) return;
    int idx = last_index_of_char_in_string('.', filename);
    if (idx < 1 || (idx == 1 && filename[0] == '.') || idx >= (fnlen - 1)) {
        ext = "";
    } else {
        int c = 0;
        for (int i = idx+1; i < fnlen; i++) {
            ext[c++] = filename[i];
        }
        ext[c] = '\0';
    }
}

unsigned short is_hidden(const char *filepath)
{
    if (filepath == NULL) return 0;
    size_t fplen = strlen(filepath);
    if (fplen < 1 || is_dot_dir(filepath)) return 0;
    char testpath[fplen];
    strncpy(testpath, filepath, fplen);
    int idx = last_index_of_char_in_string(PATH_SEPARATOR, testpath);
    while (idx > 0 && idx == fplen - 1) {
        testpath[idx] = '\0';
        idx = last_index_of_char_in_string(PATH_SEPARATOR, testpath);
        fplen--;
    }
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

void expand_path(const char *filepath, char **expanded)
{
    if (filepath == NULL) return;
    size_t fp_len = strlen(filepath);
    if (fp_len < 1) return;

    if (filepath[0] == '~') {
        wordexp_t p;
        char **w;
        int i;
        size_t exp_len = 0;

        wordexp(filepath, &p, 0);
        w = p.we_wordv;
        for (i = 0; i < p.we_wordc; i++) {
            exp_len += strlen(w[i]);
        }

        // this is probably always true here, but just in case
        if (exp_len > fp_len) {
            *expanded = (char *) realloc(*expanded, (exp_len + 1) * sizeof (char *));
            *expanded[0] = '\0';
        }

        for (i = 0; i < p.we_wordc; i++) {
            strcat(*expanded, w[i]);
        }

        wordfree(&p);
    } else {
        strncpy(*expanded, filepath, fp_len);
    }
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
    size_t end_pos = strlen(fp) - 1;
    while (fp[end_pos] == '\0') {
        end_pos--;
    }
    if (fp[end_pos] == PATH_SEPARATOR) {
        fp[end_pos] = '\0';
    }
}

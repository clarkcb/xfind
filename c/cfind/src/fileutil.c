#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <wordexp.h>
#include <printf.h>

#include "common.h"
#include "fileutil.h"

const char *DOT_DIRS[] = {".", "..", "./", "../"};

unsigned short dir_or_file_exists(const char *file_path)
{
    struct stat statbuf;
    if (stat(file_path, &statbuf) == -1) {
        if (ENOENT == errno) {
            return 0; // does not exist
        }
    }
    return 1;
}

unsigned short is_dot_dir(const char *file_path)
{
    if (file_path == NULL || strlen(file_path) < 1) return 0;
    return index_of_string_in_array(file_path, (char **) DOT_DIRS, 4) > -1;
}

long file_size(const char *file_path)
{
    struct stat st;
    if (stat(file_path, &st) == -1) {
        if (ENOENT == errno) {
            return 0; // does not exist
        }
    }
    return (long) st.st_size;
}

/*
 * get_extension - get a file_name's extension, if defined, otherwise an empty string
 *
 * NOTE: ext is expected to have enough allocated to contain the extension; the
 *       easy way to ensure that is to alloc to the size of the file_name
 */
void get_extension(const char *file_name, char *ext)
{
    if (file_name == NULL) return;
    size_t fnlen = strnlen(file_name, 1024);
    // b.c is the shortest a file_name can be with ext, so skip if shorter than 3
    if (fnlen < 3) return;
    int idx = last_index_of_char_in_string('.', file_name);
    if (idx < 1 || (idx == 1 && file_name[0] == '.') || idx >= (fnlen - 1)) {
        ext = "";
    } else {
        int c = 0;
        for (int i = idx+1; i < fnlen; i++) {
            ext[c++] = file_name[i];
        }
        ext[c] = '\0';
    }
}

unsigned short is_hidden(const char *file_path)
{
    // if NULL or empty, return false
    if (file_path == NULL) return 0;
    size_t fplen = strlen(file_path);
    if (fplen < 1) return 0;

    // if file_path has any path separators, call is_hidden on each path segment
    int sep_count = char_count_in_string(PATH_SEPARATOR, file_path);
    if (sep_count > 0) {
        int startidx = 0;
        int nextidx = 0;
        while (nextidx < strlen(file_path)) {
            if (file_path[nextidx] == PATH_SEPARATOR) {
                int seglen = nextidx - startidx;
                char seg[seglen + 1];
                memcpy(seg, &file_path[startidx], seglen);
                seg[seglen] = '\0';
                // printf("seg: \"%s\"\n", seg);
                if (is_hidden(seg)) {
                    return 1;
                }
                startidx += seglen + 1;
            } else if (nextidx == strlen(file_path) - 1) {
                int seglen = nextidx - startidx + 1;
                char seg[seglen + 1];
                memcpy(seg, &file_path[startidx], seglen);
                seg[seglen] = '\0';
                // printf("seg: \"%s\"\n", seg);
                if (is_hidden(seg)) {
                    return 1;
                }
                startidx += seglen + 1;
            }
            nextidx++;
        }

    } else {
        // check the string as an individual path element
        if (file_path[0] != '.') return 0;
        if (is_dot_dir(file_path)) return 0;
        // if it starts with a dot and isn't a dot dir, it must be a hidden dir/file
        return 1;
    }

    return 0;
}

void expand_path(const char *file_path, char **expanded)
{
    if (file_path == NULL) return;
    size_t fp_len = strlen(file_path);
    if (fp_len < 1) return;

    if (file_path[0] == '~') {
        wordexp_t p;
        char **w;
        int i;
        size_t exp_len = 0;

        wordexp(file_path, &p, 0);
        w = p.we_wordv;
        for (i = 0; i < p.we_wordc; i++) {
            exp_len += strlen(w[i]);
        }

        // this is probably always true here, but just in case
        if (exp_len > fp_len) {
            *expanded = (char *) realloc(*expanded, (exp_len + 1) * sizeof (char *));
            (*expanded)[0] = '\0';
        }

        for (i = 0; i < p.we_wordc; i++) {
            strcat(*expanded, w[i]);
        }
        (*expanded)[exp_len] = '\0';

        wordfree(&p);
    } else {
        strncpy(*expanded, file_path, fp_len);
        (*expanded)[fp_len] = '\0';
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

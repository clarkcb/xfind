#include <assert.h>
#include <string.h>
#include <sys/stat.h>

#include "pathnode.h"
#include "common.h"
#include "fileutil.h"

Path *new_path(const char *file_path)
{
    Path *path = malloc(sizeof(Path));
    assert(path != NULL);
    const size_t path_len = strnlen(file_path, MAX_PATH_LENGTH) * sizeof(char);
    int idx = index_of_char_in_string(PATH_SEPARATOR, file_path);
    if (idx == -1) {
        path->dir = "";
        path->file_name = strdup(file_path);
    } else {
        char *d = malloc(path_len);
        char *f = malloc(path_len);
        split_path(file_path, &d, &f);
        path->dir = d;
        path->file_name = f;
    }
    return path;
}

Path *new_path_from_dir_and_file_name(const char *dir, const char *file_name)
{
    Path *path = malloc(sizeof(Path));
    assert(path != NULL);
    path->dir = strdup(dir);
    path->file_name = strdup(file_name);
    return path;
}

Path *copy_path(const Path *path)
{
    Path *p = malloc(sizeof(Path));
    assert(new_path != NULL);
    if (path->dir != NULL) {
        p->dir = strdup(path->dir);
    } else {
        p->dir = "";
    }
    if (path->file_name != NULL) {
        p->file_name = strdup(path->file_name);
    } else {
        p->file_name = "";
    }
    return p;
}

int is_hidden_path(const Path *path) {
    if (path == NULL) return 0;
    if (path->dir != NULL && is_hidden(path->dir)) {
       return 1;
    }
    if (path->file_name != NULL && is_hidden(path->file_name)) {
       return 1;
    }
    return 0;
}

int path_cmp(const Path *p1, const Path *p2)
{
    if (p1 == NULL && p2 == NULL) return 0;
    if (p1 == NULL) return -1;
    if (p2 == NULL) return 1;
    const char *d1 = is_null_or_empty_string(p1->dir) == 1 ? "" : p1->dir;
    const char *d2 = is_null_or_empty_string(p2->dir) == 1 ? "" : p2->dir;
    const int d_cmp = strcmp(d1, d2);
    if (d_cmp == 0) {
        const char *f1 = is_null_or_empty_string(p1->file_name) == 1 ? "" : p1->file_name;
        const char *f2 = is_null_or_empty_string(p2->file_name) == 1 ? "" : p2->file_name;
        return strcmp(f1, f2);
    }
    return d_cmp;
}

int path_case_cmp(const Path *p1, const Path *p2)
{
    if (p1 == NULL && p2 == NULL) return 0;
    if (p1 == NULL) return -1;
    if (p2 == NULL) return 1;
    const char *d1 = is_null_or_empty_string(p1->dir) == 1 ? "" : p1->dir;
    const char *d2 = is_null_or_empty_string(p2->dir) == 1 ? "" : p2->dir;
    const int d_cmp = strcasecmp(d1, d2);
    if (d_cmp == 0) {
        const char *f1 = is_null_or_empty_string(p1->file_name) == 1 ? "" : p1->file_name;
        const char *f2 = is_null_or_empty_string(p2->file_name) == 1 ? "" : p2->file_name;
        return strcasecmp(f1, f2);
    }
    return d_cmp;
}

int path_file_name_cmp(const Path *p1, const Path *p2)
{
    if (p1 == NULL && p2 == NULL) return 0;
    if (p1 == NULL) return -1;
    if (p2 == NULL) return 1;
    const char *f1 = is_null_or_empty_string(p1->file_name) == 1 ? "" : p1->file_name;
    const char *f2 = is_null_or_empty_string(p2->file_name) == 1 ? "" : p2->file_name;
    const int f_cmp = strcmp(f1, f2);
    if (f_cmp == 0) {
        const char *d1 = is_null_or_empty_string(p1->dir) == 1 ? "" : p1->dir;
        const char *d2 = is_null_or_empty_string(p2->dir) == 1 ? "" : p2->dir;
        return strcmp(d1, d2);
    }
    return f_cmp;
}

int path_file_name_case_cmp(const Path *p1, const Path *p2)
{
    if (p1 == NULL && p2 == NULL) return 0;
    if (p1 == NULL) return -1;
    if (p2 == NULL) return 1;
    const char *f1 = is_null_or_empty_string(p1->file_name) == 1 ? "" : p1->file_name;
    const char *f2 = is_null_or_empty_string(p2->file_name) == 1 ? "" : p2->file_name;
    const int f_cmp = strcasecmp(f1, f2);
    if (f_cmp == 0) {
        const char *d1 = is_null_or_empty_string(p1->dir) == 1 ? "" : p1->dir;
        const char *d2 = is_null_or_empty_string(p2->dir) == 1 ? "" : p2->dir;
        return strcasecmp(d1, d2);
    }
    return f_cmp;
}

unsigned short path_exists(const Path *path)
{
    const size_t path_len = path_strlen(path);
    if (path_len == 0) return 0;
    char *path_s = malloc(path_len + 1);
    path_s[0] = '\0';
    path_to_string(path, path_s);
    return dir_or_file_exists(path_s);
}

unsigned short path_readable(const Path *path)
{
    const size_t path_len = path_strlen(path);
    if (path_len == 0) return 0;
    char *path_s = malloc(path_len + 1);
    path_s[0] = '\0';
    path_to_string(path, path_s);
    return dir_or_file_readable(path_s);
}

int path_stat(const Path *path, struct stat *pstat) {
    size_t path_len = path_strlen(path);
    char path_s[path_len];
    path_s[0] = '\0';
    path_to_string(path, path_s);
    struct stat fpstat;
    int res = stat(path_s, &fpstat);
    if (res != -1) {
        *pstat = fpstat;
    }
    return res;
}

size_t path_strlen(const Path *path) {
    size_t slen = 0;
    if (path != NULL) {
        if (is_null_or_empty_string(path->dir) == 0) {
            slen += strnlen(path->dir, 1024) + 1; // for path sep
        }
        if (is_null_or_empty_string(path->file_name) == 0) {
            slen += strnlen(path->file_name, 1024);
        }
    }
    return slen;
}

void path_to_string(const Path *path, char *s) {
    // assumes s has correct allocation size
    size_t path_len = path_strlen(path);
    if (path != NULL) {
        if (is_null_or_empty_string(path->dir) && is_null_or_empty_string(path->file_name)) {
            strcat(s, "");
        } else if (is_null_or_empty_string(path->dir)) {
            if (strncmp(path->file_name, ".", 5) != 0) {
                strcat(s, ".");
                strcat(s, PATH_SEPARATOR_S);
            }
            strcat(s, path->file_name);
        } else if (is_null_or_empty_string(path->file_name)) {
            strcat(s, path->dir);
        } else {
            strcat(s, path->dir);
            strcat(s, PATH_SEPARATOR_S);
            strcat(s, path->file_name);
        }
    }
    s[path_len] = '\0';
}

void destroy_path(Path *p) {
    p->dir = NULL;
    p->file_name = NULL;
    free(p);
}

PathNode *empty_path_node(void) {
    PathNode *path_node = malloc(sizeof(PathNode));
    assert(path_node != NULL);
    return path_node;
}

PathNode *new_path_node(Path *p)
{
    PathNode *path_node = malloc(sizeof(PathNode));
    assert(path_node != NULL);
    path_node->path = p;
    path_node->next = NULL;
    return path_node;
}

PathNode *new_path_node_from_dir_and_file_name(const char *dir, const char *file_name)
{
    Path *path = new_path_from_dir_and_file_name(dir, file_name);
    return new_path_node(path);
}

void add_path_to_path_node(Path *p, PathNode *path_node)
{
    if (path_node->path == NULL) {
        path_node->path = p;
    } else {
        PathNode *temp = path_node;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        temp->next = new_path_node(p);
    }
}

void add_dir_and_file_name_to_path_node(const char *dir, const char *file_name, PathNode *path_node) {
    Path *path = new_path_from_dir_and_file_name(dir, file_name);
    add_path_to_path_node(path, path_node);
}

int is_null_or_empty_path_node(const PathNode *path_node) {
    if (path_node == NULL || path_node->path == NULL) {
        return 1;
    }
    return 0;
}

size_t path_node_count(PathNode *path_node) {
    size_t node_count = 0;
    PathNode *temp = path_node;
    while (temp != NULL && temp->path != NULL) {
        node_count++;
        temp = temp->next;
    }
    return node_count;
}

size_t path_node_strlen(PathNode *path_node) {
    size_t slen = 2; // for '[' and ']'
    PathNode *temp = path_node;
    unsigned int nodecount = 0;
    while (temp != NULL && temp->path != NULL) {
        slen += path_strlen(temp->path) + 2; // for ""
        temp = temp->next;
        nodecount++;
    }
    if (nodecount > 1) {
        slen += (nodecount * 2 - 2); // for commas and spaces
    }
    return slen;
}

void path_node_to_string(PathNode *path_node, char *s) {
    // assumes s has correct allocation size
    strcat(s, "[");

    PathNode *temp = path_node;
    int node_count = 0;

    while (temp != NULL) {
        if (node_count > 0) {
            strcat(s, ", ");
        }
        strcat(s, "\"");
        char *path_s = malloc(path_strlen(temp->path) + 1);
        path_to_string(temp->path, path_s);
        strcat(s, path_s);
        strcat(s, "\"");
        temp = temp->next;
        node_count++;
    }

    strcat(s, "]");
}

void destroy_path_node(PathNode *path_node) {
    PathNode *temp = path_node;
    while (temp != NULL) {
        if (temp->path != NULL) {
            destroy_path(temp->path);
        }
        temp = temp->next;
    }
}

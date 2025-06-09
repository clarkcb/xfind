#ifndef PATHNODE_H
#define PATHNODE_H

#include <stdbool.h>
#include <stdlib.h>

typedef struct Path {
    const char *dir;
    const char *file_name;
} Path;

typedef struct PathNode {
    Path *path;
    struct PathNode *next;
} PathNode;

Path *new_path(const char *file_path);

Path *new_path_from_dir_and_file_name(const char *dir, const char *file_name);

Path *copy_path(const Path *path);

bool is_hidden_path(const Path *path);

int path_cmp(const Path *p1, const Path *p2);

int path_case_cmp(const Path *p1, const Path *p2);

int path_file_name_cmp(const Path *p1, const Path *p2);

int path_file_name_case_cmp(const Path *p1, const Path *p2);

bool path_exists(const Path *path);

bool path_readable(const Path *path);

int path_stat(const Path *path, struct stat *pstat);

size_t path_strlen(const Path *path);

void path_to_string(const Path *path, char *s);

void destroy_path(Path *p);

PathNode *empty_path_node(void);

PathNode *new_path_node(Path *p);

PathNode *new_path_node_from_dir_and_file_name(const char *dir, const char *file_name);

void add_path_to_path_node(Path *p, PathNode *path_node);

void add_dir_and_file_name_to_path_node(const char *dir, const char *file_name, PathNode *path_node);

bool is_null_or_empty_path_node(const PathNode *path_node);

size_t path_node_count(PathNode *path_node);

size_t path_node_strlen(PathNode *path_node);

void path_node_to_string(PathNode *path_node, char *s);

void destroy_path_node(PathNode *path_node);

#endif // PATHNODE_H

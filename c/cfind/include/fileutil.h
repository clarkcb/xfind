#ifndef FILEUTIL_H
#define FILEUTIL_H

#include <stdbool.h>

bool dir_or_file_exists(const char *file_path);

bool dir_or_file_readable(const char *file_path);

bool is_dot_dir(const char *file_path);

long file_size(const char *file_path);

void get_extension(const char *file_name, char *ext);

void get_file_name_without_extension(const char *file_name_with_ext, char *file_name);

bool is_hidden_name(const char *name);

bool is_hidden_path(const char *path);

void expand_path(const char *file_path, char **expanded);

void join_path(const char *path1, const char *path2, char *joined);

void split_path(const char *fp, char** p, char** f);

void normalize_path(char *path);

#endif

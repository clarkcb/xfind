#ifndef FILEUTIL_H
#define FILEUTIL_H

#if defined(_WIN32) || defined(_WIN64)
#define OS_WINDOWS 1
#define PATH_SEPARATOR '\\'
#else
#define OS_WINDOWS 0
#define PATH_SEPARATOR '/'
#endif

unsigned short dir_or_file_exists(const char *filepath);

unsigned short is_dot_dir(const char *filepath);

long file_size(const char *filepath);

void get_extension(const char *filename, char *ext);

unsigned short is_hidden(const char *filename);

void expand_path(const char *filepath, char **expanded);

void join_path(const char *path1, const char *path2, char *joined);

void split_path(const char *fp, char** p, char** f);

void normalize_path(char *path);

#endif

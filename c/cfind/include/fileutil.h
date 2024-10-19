#ifndef FILEUTIL_H
#define FILEUTIL_H

#if defined(_WIN32) || defined(_WIN64)
#define OS_WINDOWS 1
#define PATH_SEPARATOR '\\'
#define PATH_SEPARATOR_S "\\"
#else
#define OS_WINDOWS 0
#define PATH_SEPARATOR '/'
#define PATH_SEPARATOR_S "/"
#endif

unsigned short dir_or_file_exists(const char *file_path);

unsigned short dir_or_file_readable(const char *file_path);

unsigned short is_dot_dir(const char *file_path);

long file_size(const char *file_path);

void get_extension(const char *file_name, char *ext);

unsigned short is_hidden(const char *file_path);

void expand_path(const char *file_path, char **expanded);

void join_path(const char *path1, const char *path2, char *joined);

void split_path(const char *fp, char** p, char** f);

void normalize_path(char *path);

#endif

#ifndef FILEUTIL_H
#define FILEUTIL_H

#define PATH_SEPARATOR '/'

unsigned short dir_or_file_exists(const char *filepath);

void get_extension(const char *ext, char *filename);

unsigned short is_hidden(const char *filename);

void join_path(const char *path1, const char *path2, char *joined);

void split_path(const char *fp, char** p, char** f);

void normalize_path(char *path);

#endif

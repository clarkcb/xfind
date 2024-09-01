#ifndef FILETYPEMAP_H
#define FILETYPEMAP_H

#include "filetype.h"

#define FILE_TYPE_MAP_SIZE 100

typedef struct FileTypeNode {
    char *key;
    int value;
    struct FileTypeNode *next;
} FileTypeNode;

typedef struct FileTypeMap {
    FileTypeNode *buckets[FILE_TYPE_MAP_SIZE];
} FileTypeMap;

void init_file_type_map(FileTypeMap *map);

void add_entry_to_map(FileTypeMap *map, const char *key, FileType file_type);

FileType get_file_type_for_key(FileTypeMap *map, const char *key);

void print_file_type_map(const FileTypeMap *map);

void destroy_file_type_map(FileTypeMap *map);

#endif // FILETYPEMAP_H

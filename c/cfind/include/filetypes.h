#ifndef FILETYPES_H
#define FILETYPES_H

#include <stdbool.h>
#include <sqlite3.h>

#include "filetype.h"
#include "filetypemap.h"
#include "finderr.h"
#include "intnode.h"
#include "stringnode.h"

typedef struct FileTypes {
    sqlite3 *db;
    FileTypeMap *ext_type_cache;
    FileTypeMap *name_type_cache;
} FileTypes;

FileTypes *new_file_types(void);

error_t init_file_types(FileTypes *file_types);

FileType get_file_type_for_file_name(const FileTypes *file_types, const char *file_name);

FileType get_file_type_for_ext(const FileTypes *file_types, const char *ext);

FileType get_file_type(const FileTypes *file_types, const char *file_name);

bool is_archive_ext(const FileTypes *file_types, const char *ext);

bool is_archive_name(const FileTypes *file_types, const char *name);

bool is_audio_ext(const FileTypes *file_types, const char *ext);

bool is_audio_name(const FileTypes *file_types, const char *name);

bool is_binary_ext(const FileTypes *file_types, const char *ext);

bool is_binary_name(const FileTypes *file_types, const char *name);

bool is_code_ext(const FileTypes *file_types, const char *ext);

bool is_code_name(const FileTypes *file_types, const char *name);

bool is_font_ext(const FileTypes *file_types, const char *ext);

bool is_font_name(const FileTypes *file_types, const char *name);

bool is_image_ext(const FileTypes *file_types, const char *ext);

bool is_image_name(const FileTypes *file_types, const char *name);

bool is_text_ext(const FileTypes *file_types, const char *ext);

bool is_text_name(const FileTypes *file_types, const char *name);

bool is_video_ext(const FileTypes *file_types, const char *ext);

bool is_video_name(const FileTypes *file_types, const char *name);

bool is_xml_ext(const FileTypes *file_types, const char *ext);

bool is_xml_name(const FileTypes *file_types, const char *name);

FileType file_type_from_name(const char *name);

void file_type_to_name(FileType file_type, char *name);

size_t file_type_node_strlen(IntNode *file_type_node);

void file_type_node_to_string(IntNode *file_type_node, char *s);

void destroy_file_types(FileTypes *file_types);

#endif

#ifndef FILETYPES_H
#define FILETYPES_H

#include "intnode.h"
#include "stringarray.h"
#include "finderr.h"

typedef enum {
    UNKNOWN = 0,
    ARCHIVE = 1,
    BINARY  = 2,
    CODE    = 3,
    TEXT    = 4,
    XML     = 5
} FileType;


typedef struct FileTypes {
    StringArray *archive_extensions;
    StringArray *binary_extensions;
    StringArray *code_extensions;
    StringArray *text_extensions;
    StringArray *xml_extensions;
} FileTypes;

FileTypes *new_filetypes(void);

error_t get_filetypes(FileTypes *filetypes);

unsigned short is_archive_ext(const char *ext, FileTypes *filetypes);

unsigned short is_binary_ext(const char *ext, FileTypes *filetypes);

unsigned short is_code_ext(const char *ext, FileTypes *filetypes);

unsigned short is_text_ext(const char *ext, FileTypes *filetypes);

unsigned short is_xml_ext(const char *ext, FileTypes *filetypes);

FileType get_filetype_for_filename(const char *ext, FileTypes *filetypes);

FileType get_filetype_for_ext(const char *ext, FileTypes *filetypes);

FileType filetype_from_name(const char *name);

void filetype_to_name(const FileType filetype, char *name);

size_t filetype_node_strlen(IntNode *filetype_node);

void filetype_node_to_string(IntNode *filetype_node, char *s);

void destroy_filetypes(FileTypes *filetypes);

#endif

#ifndef FILETYPES_H
#define FILETYPES_H

#include "intnode.h"
#include "stringarray.h"
#include "finderr.h"

#define FILE_TYPE_NAME_ARCHIVE "archive"
#define FILE_TYPE_NAME_AUDIO "audio"
#define FILE_TYPE_NAME_BINARY "binary"
#define FILE_TYPE_NAME_CODE "code"
#define FILE_TYPE_NAME_FONT "font"
#define FILE_TYPE_NAME_IMAGE "image"
#define FILE_TYPE_NAME_TEXT "text"
#define FILE_TYPE_NAME_VIDEO "video"
#define FILE_TYPE_NAME_XML "xml"
#define FILE_TYPE_NAME_NOSEARCH "nosearch"
#define FILE_TYPE_NAME_UNKNOWN "unknown"

typedef enum {
    UNKNOWN = 0,
    ARCHIVE = 1,
    AUDIO   = 2,
    BINARY  = 3,
    CODE    = 4,
    FONT    = 5,
    IMAGE   = 6,
    TEXT    = 7,
    VIDEO   = 8,
    XML     = 9
} FileType;


typedef struct FileTypes {
    StringArray *archive_extensions;
    StringArray *archive_names;
    StringArray *audio_extensions;
    StringArray *audio_names;
    StringArray *binary_extensions;
    StringArray *binary_names;
    StringArray *code_extensions;
    StringArray *code_names;
    StringArray *font_extensions;
    StringArray *font_names;
    StringArray *image_extensions;
    StringArray *image_names;
    StringArray *text_extensions;
    StringArray *text_names;
    StringArray *video_extensions;
    StringArray *video_names;
    StringArray *xml_extensions;
    StringArray *xml_names;
} FileTypes;

FileTypes *new_file_types(void);

error_t get_file_types(FileTypes *file_types);

unsigned short is_archive_ext(const char *ext, FileTypes *file_types);

unsigned short is_archive_name(const char *name, FileTypes *file_types);

unsigned short is_audio_ext(const char *ext, FileTypes *file_types);

unsigned short is_audio_name(const char *name, FileTypes *file_types);

unsigned short is_binary_ext(const char *ext, FileTypes *file_types);

unsigned short is_binary_name(const char *name, FileTypes *file_types);

unsigned short is_code_ext(const char *ext, FileTypes *file_types);

unsigned short is_code_name(const char *name, FileTypes *file_types);

unsigned short is_font_ext(const char *ext, FileTypes *file_types);

unsigned short is_font_name(const char *name, FileTypes *file_types);

unsigned short is_image_ext(const char *ext, FileTypes *file_types);

unsigned short is_image_name(const char *name, FileTypes *file_types);

unsigned short is_text_ext(const char *ext, FileTypes *file_types);

unsigned short is_text_name(const char *name, FileTypes *file_types);

unsigned short is_video_ext(const char *ext, FileTypes *file_types);

unsigned short is_video_name(const char *name, FileTypes *file_types);

unsigned short is_xml_ext(const char *ext, FileTypes *file_types);

unsigned short is_xml_name(const char *name, FileTypes *file_types);

FileType get_file_type_for_filename(const char *filename, FileTypes *file_types);

FileType get_file_type_for_ext(const char *ext, FileTypes *file_types);

FileType file_type_from_name(const char *name);

void file_type_to_name(const FileType file_type, char *name);

size_t file_type_node_strlen(IntNode *file_type_node);

void file_type_node_to_string(IntNode *file_type_node, char *s);

void destroy_file_types(FileTypes *file_types);

#endif

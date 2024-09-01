#ifndef FILETYPE_H
#define FILETYPE_H

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

#endif // FILETYPE_H

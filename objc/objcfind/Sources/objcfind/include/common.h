#ifndef common_h
#define common_h

#import <Foundation/Foundation.h>

// file type names
#define T_ARCHIVE "archive"
#define T_AUDIO "audio"
#define T_BINARY "binary"
#define T_CODE "code"
#define T_FONT "font"
#define T_IMAGE "image"
#define T_TEXT "text"
#define T_UNKNOWN "unknown"
#define T_VIDEO "video"
#define T_XML "xml"

// sort-by names
#define S_FILEPATH "filepath"
#define S_FILENAME "filename"
#define S_NAME "name"
#define S_FILESIZE "filesize"
#define S_SIZE "size"
#define S_FILETYPE "filetype"
#define S_TYPE "type"
#define S_LASTMOD "lastmod"

// the accepted date format
#define DATE_FORMAT "yyyy-MM-dd"

// file type enum
typedef enum {
    FileTypeUnknown = -1,
    FileTypeArchive = 0,
    FileTypeAudio,
    FileTypeBinary,
    FileTypeCode,
    FileTypeFont,
    FileTypeImage,
    FileTypeText,
    FileTypeVideo,
    FileTypeXml
} FileType;

// sort-by enum
typedef enum {
    SortByFilePath = 1,
    SortByFileName,
    SortByFileSize,
    SortByFileType,
    SortByLastMod
} SortBy;

// ArgTokenType enum
typedef enum {
    ArgTokenTypeUnknown = 0,
    ArgTokenTypeBool = 1,
    ArgTokenTypeStr,
    ArgTokenTypeInt
} ArgTokenType;

// common functions
void logMsg(NSString *s);
void logError(NSString *s);
void setError(NSError **e, NSString *msg);
NSString * boolToNSString(BOOL b);
NSString * arrayToNSString(NSArray *arr);
NSString* dateToNSString(NSDate *date);
NSDate* stringToNSDate(NSString *dateStr);

#endif /* common_h */

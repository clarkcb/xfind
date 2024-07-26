#import "FindConfig.h"
#import "FileTypes.h"
#import "FileUtil.h"

@interface FileTypes ()


@property NSString *archive;
@property NSString *audio;
@property NSString *binary;
@property NSString *code;
@property NSString *font;
@property NSString *image;
@property NSString *text;
@property NSString *unknown;
@property NSString *video;
@property NSString *xml;

@property sqlite3 *db;
@property NSMutableDictionary *extTypeCache;

@end

@implementation FileTypes

- (instancetype) init {
    self = [super init];
    if (self) {
        self.archive = [NSString stringWithUTF8String:T_ARCHIVE];
        self.audio = [NSString stringWithUTF8String:T_AUDIO];
        self.binary = [NSString stringWithUTF8String:T_BINARY];
        self.code = [NSString stringWithUTF8String:T_CODE];
        self.font = [NSString stringWithUTF8String:T_FONT];
        self.image = [NSString stringWithUTF8String:T_IMAGE];
        self.text = [NSString stringWithUTF8String:T_TEXT];
        self.unknown = [NSString stringWithUTF8String:T_UNKNOWN];
        self.video = [NSString stringWithUTF8String:T_VIDEO];
        self.xml = [NSString stringWithUTF8String:T_XML];
        
        NSString *xfindDbPath = getXfindDbPath();
        sqlite3 *db;
        int rc = sqlite3_open_v2([xfindDbPath UTF8String], &db, SQLITE_OPEN_READONLY, nil);
        if (rc == SQLITE_OK && db != nil) {
            self.db = db;
        }
        self.extTypeCache = [[NSMutableDictionary alloc] initWithCapacity:10];
    }
    return self;
}

+ (FileType) fromName:(NSString*)typeName {
    NSString *lname = [typeName lowercaseString];
    if (lname == [NSString stringWithUTF8String:T_ARCHIVE]) {
        return FileTypeArchive;
    }
    if (lname == [NSString stringWithUTF8String:T_AUDIO]) {
        return FileTypeAudio;
    }
    if (lname == [NSString stringWithUTF8String:T_BINARY]) {
        return FileTypeBinary;
    }
    if (lname == [NSString stringWithUTF8String:T_CODE]) {
        return FileTypeCode;
    }
    if (lname == [NSString stringWithUTF8String:T_FONT]) {
        return FileTypeFont;
    }
    if (lname == [NSString stringWithUTF8String:T_IMAGE]) {
        return FileTypeImage;
    }
    if (lname == [NSString stringWithUTF8String:T_TEXT]) {
        return FileTypeText;
    }
    if (lname == [NSString stringWithUTF8String:T_VIDEO]) {
        return FileTypeVideo;
    }
    if (lname == [NSString stringWithUTF8String:T_XML]) {
        return FileTypeXml;
    }
    return FileTypeUnknown;
}

+ (NSString*) toName:(FileType)fileType {
    if (fileType == FileTypeArchive) {
        return [NSString stringWithUTF8String:T_ARCHIVE];
    }
    if (fileType == FileTypeAudio) {
        return [NSString stringWithUTF8String:T_AUDIO];
    }
    if (fileType == FileTypeBinary) {
        return [NSString stringWithUTF8String:T_BINARY];
    }
    if (fileType == FileTypeCode) {
        return [NSString stringWithUTF8String:T_CODE];
    }
    if (fileType == FileTypeFont) {
        return [NSString stringWithUTF8String:T_FONT];
    }
    if (fileType == FileTypeImage) {
        return [NSString stringWithUTF8String:T_IMAGE];
    }
    if (fileType == FileTypeText) {
        return [NSString stringWithUTF8String:T_TEXT];
    }
    if (fileType == FileTypeVideo) {
        return [NSString stringWithUTF8String:T_VIDEO];
    }
    if (fileType == FileTypeXml) {
        return [NSString stringWithUTF8String:T_XML];
    }
    return [NSString stringWithUTF8String:T_UNKNOWN];
}

- (FileType) getFileTypeForQuery:(NSString*)query andElem:(NSString*)elem {
    FileType fileType = FileTypeUnknown;
    sqlite3_stmt *stmt;
    sqlite3_prepare_v2(self.db, [query UTF8String], -1, &stmt, NULL);
    int rc = sqlite3_bind_text(stmt, 1, [elem UTF8String], -1, SQLITE_TRANSIENT);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "error: %s\n", sqlite3_errmsg(self.db));
    }
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        fileType = sqlite3_column_int(stmt, 0) - 2;
    }
    sqlite3_finalize(stmt);
    return fileType;
}

- (FileType) getFileTypeForFileName:(NSString *)fileName {
    NSString *query = @"select file_type_id from file_name where name=?";
    return [self getFileTypeForQuery:query andElem:fileName];
}

- (FileType) getFileTypeForExtension:(NSString *)fileExt {
    if (self.extTypeCache[fileExt]) {
        NSNumber* n = self.extTypeCache[fileExt];
        return [n intValue];
    }
    NSString *query = @"select file_type_id from file_extension where extension=?";
    FileType fileType = [self getFileTypeForQuery:query andElem:fileExt];
    [self.extTypeCache setObject:[NSNumber numberWithInt:fileType] forKey:fileExt];
    return fileType;
}

- (FileType) getFileType:(NSString*)fileName {
    if (fileName == nil || fileName.length == 0) {
        return FileTypeUnknown;
    }
    FileType fileType = [self getFileTypeForFileName:fileName];
    if (fileType != FileTypeUnknown) {
        return fileType;
    }
    NSString *ext = [FileUtil getExtension:fileName];
    return [self getFileTypeForExtension:ext];
}

- (BOOL) isArchiveFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeArchive;
}

- (BOOL) isAudioFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeAudio;
}

- (BOOL) isBinaryFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeBinary;
}

- (BOOL) isCodeFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeCode;
}

- (BOOL) isFontFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeFont;
}

- (BOOL) isImageFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeImage;
}

- (BOOL) isTextFile:(NSString*)fileName {
    FileType fileType = [self getFileType:fileName];
    return fileType == FileTypeText || fileType == FileTypeCode || fileType == FileTypeXml;
}

- (BOOL) isVideoFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeVideo;
}

- (BOOL) isXmlFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeXml;
}

- (BOOL) isUnknownFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeUnknown;
}

@end

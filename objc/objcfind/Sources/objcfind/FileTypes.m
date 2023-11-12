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


@property NSDictionary<NSString*,NSSet<NSString*>*> *fileTypeExtDict;
@property NSDictionary<NSString*,NSSet<NSString*>*> *fileTypeNameDict;

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
        NSArray<NSDictionary<NSString*,NSSet<NSString*>*>*> *ftArr = [self fileTypesFromJson];
        self.fileTypeExtDict = ftArr[0];
        self.fileTypeNameDict = ftArr[1];
    }
    return self;
}

- (NSArray<NSDictionary<NSString*,NSSet<NSString*>*>*>*) fileTypesFromJson {
    NSMutableString *fileTypesJsonPath = [NSMutableString stringWithString:getXfindSharedPath()];
    [fileTypesJsonPath appendString:@"/filetypes.json"];

    NSMutableDictionary *fileTypeExtDict = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *fileTypeNameDict = [[NSMutableDictionary alloc] init];

    if (![[NSFileManager defaultManager] fileExistsAtPath:fileTypesJsonPath]) {
        return nil;
    }

    NSData *data = [NSData dataWithContentsOfFile:fileTypesJsonPath];

    if (NSClassFromString(@"NSJSONSerialization")) {
        NSError *error = nil;
        id jsonObject = [NSJSONSerialization
                         JSONObjectWithData:data
                         options:0
                         error:&error];

        if (error) { /* JSON was malformed, act appropriately here */ }

        if ([jsonObject isKindOfClass:[NSDictionary class]]) {
            NSArray *fileTypes = jsonObject[@"filetypes"];
            for (NSDictionary *typeDict in fileTypes) {
                NSString *type = typeDict[@"type"];
                NSArray *extensions = typeDict[@"extensions"];
                fileTypeExtDict[type] = [NSSet setWithArray:extensions];
                NSArray *names = typeDict[@"names"];
                fileTypeNameDict[type] = [NSSet setWithArray:names];
            }
        }
    }
    NSMutableArray<NSDictionary<NSString*,NSSet<NSString*>*>*> *fileTypesDictionaries = [[NSMutableArray alloc] init];
    NSDictionary *ftExtDict = [NSDictionary dictionaryWithDictionary:fileTypeExtDict];
    [fileTypesDictionaries addObject:ftExtDict];

    NSDictionary *ftNameDict = [NSDictionary dictionaryWithDictionary:fileTypeNameDict];
    [fileTypesDictionaries addObject:ftNameDict];

    return [NSArray arrayWithArray:fileTypesDictionaries];

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

- (FileType) getFileType:(NSString*)fileName {
    // most specific first
    if ([self isCodeFile:fileName]) {
        return FileTypeCode;
    }
    if ([self isArchiveFile:fileName]) {
        return FileTypeArchive;
    }
    if ([self isAudioFile:fileName]) {
        return FileTypeAudio;
    }
    if ([self isFontFile:fileName]) {
        return FileTypeFont;
    }
    if ([self isImageFile:fileName]) {
        return FileTypeImage;
    }
    if ([self isVideoFile:fileName]) {
        return FileTypeVideo;
    }
    // most general last
    if ([self isXmlFile:fileName]) {
        return FileTypeXml;
    }
    if ([self isTextFile:fileName]) {
        return FileTypeText;
    }
    if ([self isBinaryFile:fileName]) {
        return FileTypeBinary;
    }
    return FileTypeUnknown;
}

- (BOOL) isFileOfType:(NSString*)fileName type:(NSString*)typeName {
    if ([self.fileTypeNameDict[typeName] containsObject:fileName]) {
        return true;
    }
    NSString *ext = [FileUtil getExtension:fileName];
    return self.fileTypeExtDict[typeName] != nil &&
    [self.fileTypeExtDict[typeName] containsObject:ext];
}

- (BOOL) isArchiveFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.archive];
}

- (BOOL) isAudioFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.audio];
}

- (BOOL) isBinaryFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.binary];
}

- (BOOL) isCodeFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.code];
}

- (BOOL) isFontFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.font];
}

- (BOOL) isImageFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.image];
}

- (BOOL) isTextFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.text];
}

- (BOOL) isVideoFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.video];
}

- (BOOL) isXmlFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.xml];
}

- (BOOL) isUnknownFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeUnknown;
}

@end

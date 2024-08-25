#import <XCTest/XCTest.h>
#import "FileTypes.h"

@interface FileTypesTests : XCTestCase
@property FileTypes *fileTypes;
@end

@implementation FileTypesTests

- (void)setUp {
    [super setUp];
    self.fileTypes = [[FileTypes alloc] init];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testArchiveFiles {
    NSArray<NSString*> *exts = @[@"7z", @"arj", @"bz2", @"cpio", @"ear", @"gz", @"hqx",
                                 @"jar", @"pax", @"rar", @"sit", @"sitx", @"tar", @"tgz", @"war",
                                 @"zip", @"zipx", @"Z"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"archive.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeArchive);
        XCTAssert([self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
        XCTAssert(![self.fileTypes isFontFile:fileName]);
        XCTAssert(![self.fileTypes isImageFile:fileName]);
        XCTAssert(![self.fileTypes isVideoFile:fileName]);
    }
}

- (void)testAudioFiles {
    NSArray<NSString*> *exts = @[@"aac", @"cda", @"mid", @"midi", @"mp3", @"oga", @"opus", @"wav", @"weba"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"audio.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeAudio);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert([self.fileTypes isAudioFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
        XCTAssert(![self.fileTypes isFontFile:fileName]);
        XCTAssert(![self.fileTypes isImageFile:fileName]);
        XCTAssert(![self.fileTypes isVideoFile:fileName]);
    }
}

- (void)testBinaryFiles {
    NSArray<NSString*> *exts = @[@"a", @"beam", @"chm", @"class", @"com", @"dat",
                                 @"dll", @"doc", @"dot", @"dylib", @"exe", @"hlp", @"lib", @"mdb",
                                 @"pdb", @"pot", @"so"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"binary.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeBinary);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isAudioFile:fileName]);
        XCTAssert([self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
        XCTAssert(![self.fileTypes isFontFile:fileName]);
        XCTAssert(![self.fileTypes isImageFile:fileName]);
        XCTAssert(![self.fileTypes isVideoFile:fileName]);
    }
}

- (void)testCodeFiles {
    NSArray<NSString*> *exts = @[@"asp", @"bas", @"bash", @"bat", @"c", @"clj", @"cpp", @"cs",
                                 @"css", @"erl", @"fs", @"go", @"groovy", @"h", @"hpp", @"hs",
                                 @"htm", @"html", @"java", @"js", @"m", @"php", @"py", @"rb",
                                 @"rc", @"scala", @"sh",  @"swift", @"ts", @"vb"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"code.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeCode);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isAudioFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert([self.fileTypes isCodeFile:fileName]);
        XCTAssert(![self.fileTypes isFontFile:fileName]);
        XCTAssert(![self.fileTypes isImageFile:fileName]);
        XCTAssert(![self.fileTypes isVideoFile:fileName]);
    }
}

- (void)testFontFiles {
    NSArray<NSString*> *exts = @[@"eot", @"otf", @"tte", @"ttf", @"woff", @"woff2"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"font.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeFont);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isAudioFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
        XCTAssert([self.fileTypes isFontFile:fileName]);
        XCTAssert(![self.fileTypes isImageFile:fileName]);
        XCTAssert(![self.fileTypes isVideoFile:fileName]);
    }
}

- (void)testImageFiles {
//    NSArray<NSString*> *exts = @[@"ai", @"avif", @"bmp", @"bpg", @"cur", @"dib", @"drw"];
    NSArray<NSString*> *exts = @[@"avif", @"bmp", @"bpg", @"cur", @"dib", @"drw"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"image.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeImage);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isAudioFile:fileName]);
//        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
        XCTAssert(![self.fileTypes isFontFile:fileName]);
        XCTAssert([self.fileTypes isImageFile:fileName]);
        XCTAssert(![self.fileTypes isVideoFile:fileName]);
    }
}

- (void)testTextFiles {
    NSArray<NSString*> *exts = @[@"cfg", @"conf", @"csv", @"ddl", @"ini", @"log",
                                 @"markdown", @"md", @"po", @"properties", @"sgm", @"sgml",
                                 @"txt" , @"yml"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"text.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeText);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
        XCTAssert([self.fileTypes isTextFile:fileName]);
        XCTAssert(![self.fileTypes isFontFile:fileName]);
        XCTAssert(![self.fileTypes isVideoFile:fileName]);
    }
}

- (void)testVideoFiles {
    NSArray<NSString*> *exts = @[@"avi", @"mov", @"mp4", @"mpeg", @"ogv", @"webm"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"video.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeVideo);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isAudioFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
        XCTAssert(![self.fileTypes isFontFile:fileName]);
        XCTAssert(![self.fileTypes isImageFile:fileName]);
        XCTAssert([self.fileTypes isVideoFile:fileName]);
    }
}

@end

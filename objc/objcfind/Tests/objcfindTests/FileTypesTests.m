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
    NSArray<NSString*> *exts = @[@"7z", @"arj", @"bin", @"bz2", @"cpio", @"ear", @"gz", @"hqx",
                                 @"jar", @"pax", @"rar", @"sit", @"sitx", @"tar", @"tgz", @"war",
                                 @"zip", @"zipx", @"Z"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"archive.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeArchive);
        XCTAssert([self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
    }
}

- (void)testBinaryFiles {
    NSArray<NSString*> *exts = @[@"a", @"ai", @"beam", @"chm", @"class", @"com", @"dat",
                                 @"dll", @"doc", @"dot", @"dylib", @"exe", @"hlp", @"lib", @"mdb",
                                 @"pdb", @"pot", @"so"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"binary.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeBinary);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert([self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
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
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert([self.fileTypes isCodeFile:fileName]);
    }
}

- (void)testTextFiles {
    NSArray<NSString*> *exts = @[@"cfg", @"cmake", @"conf", @"csv", @"ddl", @"ics", @"ini", @"log",
                                 @"markdown", @"md", @"po", @"properties", @"scc", @"sgm", @"sgml",
                                 @"txt", @"yml"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"text.%@", x];
         XCTAssert([self.fileTypes getFileType:fileName] == FileTypeText);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
        XCTAssert([self.fileTypes isTextFile:fileName]);
    }
}

@end

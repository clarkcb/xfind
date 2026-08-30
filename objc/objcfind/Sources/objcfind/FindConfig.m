#import "FindConfig.h"

@implementation FindConfig

- (instancetype) init {
    self = [super init];
    if (self) {
        NSString *homePath = [[[NSProcessInfo processInfo] environment] objectForKey:@"HOME"];
        NSString *defaultXFindConfigDir = [NSString pathWithComponents:@[homePath, @".config", @"xfind"]];
        NSString *xfindConfigDir = [[[NSProcessInfo processInfo] environment] objectForKey:@"XFIND_CONFIG_DIR"];
        if (xfindConfigDir == nil) {
            xfindConfigDir = defaultXFindConfigDir;
        }
        NSString *defaultXFindPath = [NSString pathWithComponents:@[homePath, @"src", @"xfind"]];
        NSString *xfindPath = [[[NSProcessInfo processInfo] environment] objectForKey:@"XFIND_PATH"];
        if (xfindPath == nil) {
            xfindPath = defaultXFindPath;
        }
        self.xfindPath = xfindPath;
        self.fileTypesPath = [NSString pathWithComponents:@[xfindPath, @"shared", @"filetypes.json"]];
        self.findOptionsPath = [NSString pathWithComponents:@[xfindPath, @"shared", @"findoptions.json"]];
        self.defaultFindSettingsPath = [NSString pathWithComponents:@[xfindConfigDir, @"settings.json"]];
    }
    return self;
}

@end

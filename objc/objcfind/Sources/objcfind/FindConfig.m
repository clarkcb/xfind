#import "FindConfig.h"

@implementation FindConfig

- (instancetype) init {
    self = [super init];
    if (self) {
        NSString *homePath = [[[NSProcessInfo processInfo] environment] objectForKey:@"HOME"];
        NSString *xfindPath = [[[NSProcessInfo processInfo] environment] objectForKey:@"XFIND_PATH"];
        if (xfindPath == nil) {
            xfindPath = [NSString pathWithComponents:@[homePath, @"src", @"xfind"]];
        }
        self.xfindPath = xfindPath;
        self.fileTypesPath = [NSString pathWithComponents:@[xfindPath, @"shared", @"filetypes.json"]];
        self.findOptionsPath = [NSString pathWithComponents:@[xfindPath, @"shared", @"findoptions.json"]];
        self.defaultFindSettingsPath = [NSString pathWithComponents:@[homePath, @".config", @"xfind", @"settings.json"]];
    }
    return self;
}

@end

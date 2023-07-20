#import "FindConfig.h"

NSString* getXfindPath() {
    NSString *xfindPath = [[[NSProcessInfo processInfo] environment] objectForKey:@"XFIND_PATH"];
    if (xfindPath == nil) {
        xfindPath = [[[[NSProcessInfo processInfo] environment] objectForKey:@"HOME"]
                     stringByAppendingString:@"/src/xfind"];
    }
    return xfindPath;
}

NSString* getXfindSharedPath() {
    NSString *xfindPath = getXfindPath();
    return [xfindPath stringByAppendingPathComponent:@"shared"];
}

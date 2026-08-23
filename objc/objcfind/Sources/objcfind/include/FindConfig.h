#ifndef FindConfig_h
#define FindConfig_h

#import <Foundation/Foundation.h>

@interface FindConfig : NSObject

@property NSString *xfindPath;
@property NSString *fileTypesPath;
@property NSString *findOptionsPath;
@property NSString *defaultFindSettingsPath;

- (instancetype) init;

@end

#endif /* FindConfig_h */

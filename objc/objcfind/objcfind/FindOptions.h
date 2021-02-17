#import <Foundation/Foundation.h>
#import "FindOption.h"
#import "FindSettings.h"

@interface FindOptions : NSObject

- (NSArray<FindOption*>*) findOptionsFromJson;
- (FindSettings*) settingsFromArgs:(NSArray*)args error:(NSError**)error;
- (void) settingsFromData:(NSData *)data settings:(FindSettings *)settings;
- (NSString*) getUsageString;
- (void) usage:(int)code;

@end

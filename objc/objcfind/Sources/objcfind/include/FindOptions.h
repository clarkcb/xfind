#ifndef FindOptions_h
#define FindOptions_h

#import <Foundation/Foundation.h>
#import "FindOption.h"
#import "FindSettings.h"

@interface FindOptions : NSObject

- (NSArray<FindOption*>*) findOptionsFromJson;
- (FindSettings*) settingsFromArgs:(NSArray*)args error:(NSError**)error;
- (void) updateSettingsFromArgs:(FindSettings *)settings args:(NSArray *)args error:(NSError **)error;
- (FindSettings*) settingsFromData:(NSData *)data error:(NSError **)error;
- (void) updateSettingsFromData:(FindSettings *)settings data:(NSData *)data error:(NSError **)error;
- (FindSettings *) settingsFromFile:(NSString *)filePath error:(NSError **)error;
- (void) updateSettingsFromFile:(FindSettings *)settings filePath:(NSString *)filePath error:(NSError **)error;
- (NSString*) getUsageString;
- (void) usage:(int)code;

@end

#endif /* FindOptions_h */

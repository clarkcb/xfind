#import "ArgToken.h"

@implementation ArgToken

- (instancetype) initWithName:(NSString *)name type:(ArgTokenType)type value:(NSObject *)value {
    self = [super init];
    if (self) {
        self.name = name;
        self.type = type;
        self.value = value;
    }
    return self;
}

@end

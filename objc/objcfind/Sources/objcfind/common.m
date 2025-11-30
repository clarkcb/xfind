#import "consolecolor.h"
#import "common.h"
#import "Regex.h"

void logMsg(NSString *s) {
    //NSLog(@"%@", s);
    printf("%s\n", [s UTF8String]);
}

void logError(NSString *s) {
    //NSLog(@"ERROR: %@", s);
    fprintf(stderr, "ERROR: %s\n", [s UTF8String]);
}

void logErrorColor(NSString *s, BOOL colorize) {
    if (colorize) {
        NSMutableString *err = [NSMutableString string];
        [err appendFormat:@"%s", BOLD_RED];
        [err appendString:@"ERROR: "];
        [err appendString:s];
        [err appendFormat:@"%s", ANSI_RESET];
        fprintf(stderr, "%s\n", [err UTF8String]);
    } else {
        fprintf(stderr, "ERROR: %s\n", [s UTF8String]);
    }
}

void setError(NSError **error, NSString *msg) {
    NSString *domain = msg;
    NSDictionary *userInfo = [[NSDictionary alloc] init];
    *error = [NSError errorWithDomain:domain code:500 userInfo:userInfo];
}

NSString* boolToNSString(BOOL b) {
    switch (b) {
        case 1:
            return @"true";
        default:
            return @"false";
    }
}

NSString* arrayToNSString(NSArray *arr) {
    NSMutableString *arrString = [NSMutableString stringWithString:@"["];
    for (int i=0; i < [arr count]; i++) {
        if (i > 0) {
            [arrString appendString:@", "];
        }
        if ([arr[i] isKindOfClass:[NSString class]]) {
            [arrString appendFormat:@"\"%@\"", arr[i]];
        } else if ([arr[i] isKindOfClass:[Regex class]]) {
            [arrString appendFormat:@"\"%@\"", ((Regex*)arr[i]).pattern];
        } else {
            [arrString appendFormat:@"%@", arr[i]];
        }
    }
    [arrString appendString:@"]"];
    return [NSString stringWithString:arrString];
}

NSString* dateToNSString(NSDate *date) {
    if (date == nil) {
        return @"";
    }
    NSDateFormatter *dateFormat = [[NSDateFormatter alloc] init];
    [dateFormat setDateFormat:[NSString stringWithUTF8String:DATE_FORMAT]];
    return [dateFormat stringFromDate:date];
}

NSDate* stringToNSDate(NSString *dateStr) {
    if (dateStr == nil) {
        return nil;
    }
    NSDateFormatter *dateFormat = [[NSDateFormatter alloc] init];
    [dateFormat setDateFormat:[NSString stringWithUTF8String:DATE_FORMAT]];
    return [dateFormat dateFromString:dateStr];
}

NSString* colorToConsoleColor(Color color) {
    switch (color) {
        case ColorBlack:
            return [NSString stringWithUTF8String:ANSI_BLACK];
        case ColorRed:
            return [NSString stringWithUTF8String:ANSI_RED];
        case ColorGreen:
            return [NSString stringWithUTF8String:ANSI_GREEN];
        case ColorYellow:
            return [NSString stringWithUTF8String:ANSI_YELLOW];
        case ColorBlue:
            return [NSString stringWithUTF8String:ANSI_BLUE];
        case ColorMagenta:
            return [NSString stringWithUTF8String:ANSI_MAGENTA];
        case ColorCyan:
            return [NSString stringWithUTF8String:ANSI_CYAN];
        case ColorWhite:
            return [NSString stringWithUTF8String:ANSI_WHITE];
    }
}

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

NSArray<NSString*> *filterStrings(NSArray<NSString*> *stringArray, StringPredicate predicate) {
    NSMutableArray<NSString *> *filteredArray = [NSMutableArray array];
    for (NSString *string in stringArray) {
        if (predicate(string)) {
            [filteredArray addObject:string];
        }
    }
    return [filteredArray copy];
}

int findString(NSArray<NSString*> *stringArray, StringPredicate predicate) {
    for (int i=0; i < stringArray.count; i++) {
        if (predicate(stringArray[i])) {
            return i;
        }
    }
    return -1;
}

int indexOfCharInString(NSString *s, char c) {
    NSRange range = [s rangeOfString:[NSString stringWithFormat:@"%c", c]];
    if (range.location != NSNotFound) {
        NSUInteger indexOfChar = range.location;
        return (int)range.location;
    }
    return -1;
}

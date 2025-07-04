//
//  FileResultFormatter.m
//  objcfind
//
//  Created by Cary Clark on 5/10/25.
//

#import <Foundation/Foundation.h>
#import "FileResultFormatter.h"
#import "FileUtil.h"

@implementation FileResultFormatter

- (instancetype) initWithSettings:(FindSettings*)settings {
    self = [super init];
    if (self) {
        // necessary to reference other methods in self from constructor
        __weak typeof(self) weakSelf = self;

        self.settings = settings;
        if (settings.colorize && settings.inDirPatterns.count > 0) {
            self.formatDirPath = ^( NSString* dirPath) { return [weakSelf formatDirPathWithColor:dirPath]; };
        } else {
            self.formatDirPath = ^( NSString* dirPath) { return dirPath; };
        }
        if (settings.colorize && (settings.inExtensions.count > 0 || settings.inFilePatterns.count > 0)) {
            self.formatFileName = ^( NSString* fileName) { return [weakSelf formatFileNameWithColor:fileName]; };
        } else {
            self.formatFileName = ^( NSString* fileName) { return fileName; };
        }
    }
    return self;
}

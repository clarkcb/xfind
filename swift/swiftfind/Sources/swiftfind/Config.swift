//
//  Config.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public struct Config {
    public let xfindPath: String
    public let sharedPath: String
    public let fileTypesPath: String
    public let findOptionsPath: String

    public init() {
        if let xfindEnvPath = ProcessInfo.processInfo.environment["XFIND_PATH"] {
            self.xfindPath = xfindEnvPath
        } else {
            self.xfindPath = "\(NSHomeDirectory())/src/xfind"
        }
        self.sharedPath = "\(xfindPath)/shared"
        self.fileTypesPath = "\(sharedPath)/filetypes.json"
        self.findOptionsPath = "\(sharedPath)/findoptions.json"
    }
}

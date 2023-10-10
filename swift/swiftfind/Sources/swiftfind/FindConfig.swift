//
//  FindConfig.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public struct FindConfig {
    public let xfindPath: String
    public let sharedPath: String
    public let fileTypesPath: String
    public let findOptionsPath: String

    public init() {
        if let xfindEnvPath = ProcessInfo.processInfo.environment["XFIND_PATH"] {
            xfindPath = xfindEnvPath
        } else {
            xfindPath = "\(NSHomeDirectory())/src/xfind"
        }
        sharedPath = "\(xfindPath)/shared"
        fileTypesPath = "\(sharedPath)/filetypes.json"
        findOptionsPath = "\(sharedPath)/findoptions.json"
    }
}

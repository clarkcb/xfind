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
    public let defaultFindSettingsPath: String

    public init() {
        var defaultXFindConfigDir = "\(NSHomeDirectory())/.config/xfind"
        var xFindConfigDir: String
        if let xFindEnvConfigDir = ProcessInfo.processInfo.environment["XFIND_CONFIG_DIR"] {
            xFindConfigDir = xFindEnvConfigDir
        } else {
            xFindConfigDir = defaultXFindConfigDir
        }
        var defaultXFindPath = "\(NSHomeDirectory())/src/xfind"
        if let xFindEnvPath = ProcessInfo.processInfo.environment["XFIND_PATH"] {
            xfindPath = xFindEnvPath
        } else {
            xfindPath = defaultXFindPath
        }
        sharedPath = "\(xfindPath)/shared"
        fileTypesPath = "\(sharedPath)/filetypes.json"
        findOptionsPath = "\(sharedPath)/findoptions.json"
        defaultFindSettingsPath = "\(xFindConfigDir)/settings.json"
    }
}

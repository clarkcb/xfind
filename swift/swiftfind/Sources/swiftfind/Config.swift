//
//  Config.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public enum Config {
    // static let xfindPath = NSString(string: "~/src/xfind").stringByExpandingTildeInPath
    public static let xfindPath = "\(NSHomeDirectory())/src/xfind"
    public static let sharedPath = "\(xfindPath)/shared"
    // public static let fileTypesPath = "\(sharedPath)/filetypes.xml"
    public static let fileTypesPath = "\(sharedPath)/filetypes.json"
    // public static let findOptionsPath = "\(sharedPath)/findoptions.xml"
    public static let findOptionsPath = "\(sharedPath)/findoptions.json"
}

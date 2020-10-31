//
//  Config.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

class Config {
    // static let xsearchPath = NSString(string: "~/src/xsearch").stringByExpandingTildeInPath
    static let xsearchPath = "\(NSHomeDirectory())/src/xsearch"
    static let sharedPath = "\(xsearchPath)/shared"
    static let fileTypesPath = "\(sharedPath)/filetypes.xml"
    static let searchOptionsPath = "\(sharedPath)/searchoptions.xml"
}

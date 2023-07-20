//
//  FileResult.swift
//  swiftfind
//
//  Created by Cary Clark on 6/2/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public struct FileResult {
    public let containerSeparator = "!"
    public let containers: [String]
    public let filePath: String
    public let fileType: FileType
    public let stat: [FileAttributeKey: Any]?

    public init(filePath: String, fileType: FileType, stat: [FileAttributeKey: Any]?=nil) {
        self.filePath = filePath
        self.fileType = fileType
        self.stat = stat
        containers = []
    }

    public var description: String {
        "\(containers.isEmpty ? "" : containers.joined(separator: containerSeparator) + containerSeparator)" +
            filePath
    }
}

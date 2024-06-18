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
    public let fileSize: UInt64
    public let lastMod: Date?

    public init(filePath: String, fileType: FileType, fileSize: UInt64, lastMod: Date?=nil) {
        self.filePath = filePath
        self.fileType = fileType
        self.fileSize = fileSize
        self.lastMod = lastMod
        containers = []
    }

    public var description: String {
        "\(containers.isEmpty ? "" : containers.joined(separator: containerSeparator) + containerSeparator)" +
            filePath
    }
}

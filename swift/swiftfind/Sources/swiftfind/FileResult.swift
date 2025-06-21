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

public class FileResultFormatter {
    let settings: FindSettings
    public var formatDirPath = { (dirPath: String) in dirPath }
    public var formatFileName = { (fileName: String) in fileName }

    public init(settings: FindSettings) {
        self.settings = settings
        if settings.colorize {
            if !settings.inDirPatterns.isEmpty {
                formatDirPath = formatDirPathWithColor
            }
            if !settings.inExtensions.isEmpty || !settings.inFilePatterns.isEmpty {
                formatFileName = formatFileNameWithColor
            }
        }
    }

    public func colorize(_ s: String, _ matchStartIndex: Int, _ matchEndIndex: Int) -> String {
        let strMatchStartIndex = s.index(s.startIndex, offsetBy: matchStartIndex)
        let strMatchEndIndex = s.index(s.startIndex, offsetBy: matchEndIndex)
        var prefix = ""
        if matchStartIndex > 0 {
            prefix = String(s.prefix(matchStartIndex))
        }
        var suffix = ""
        if matchEndIndex < s.count {
            suffix = String(s[strMatchEndIndex...])
        }
        return prefix +
        Color.GREEN +
        String(s[strMatchStartIndex ..< strMatchEndIndex]) +
        Color.RESET +
        suffix
    }

    private func formatDirPathWithColor(_ dirPath: String) -> String {
        var formattedDirPath = dirPath
        if formattedDirPath == "" {
            formattedDirPath = "."
        } else {
            for p in settings.inDirPatterns {
                let match = p.firstMatch(formattedDirPath)
                if match != nil {
                    formattedDirPath = colorize(formattedDirPath, match!.range.lowerBound, match!.range.upperBound)
                    break
                }
            }
        }
        return formattedDirPath
    }

    private func formatFileNameWithColor(_ fileName: String) -> String {
        var formattedFileName = fileName
        for p in settings.inFilePatterns {
            let match = p.firstMatch(formattedFileName)
            if match != nil {
                formattedFileName = colorize(formattedFileName, match!.range.lowerBound, match!.range.upperBound)
                break
            }
        }
        if !settings.inExtensions.isEmpty {
            let idx = formattedFileName.lastIndex(of: ".")
            if idx != nil && idx!.encodedOffset > 0 && idx!.encodedOffset < formattedFileName.count - 1 {
                formattedFileName = colorize(formattedFileName, idx!.encodedOffset + 1, formattedFileName.count)
            }
        }

        return formattedFileName
    }

    public func formatFilePath(_ filePath: String) -> String {
        let (parent, fileName) = FileUtil.splitPath(filePath)
        let formattedParent = formatDirPath(parent)
        let formattedFileName = formatFileName(fileName)
        return FileUtil.joinPath(formattedParent, childPath: formattedFileName)
    }

    public func formatFileResult(_ result: FileResult) -> String {
        return formatFilePath(result.filePath)
    }
}

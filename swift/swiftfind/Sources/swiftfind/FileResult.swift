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

    public func colorize(_ s: String, _ matchStartIndex: Int, _ matchEndIndex: Int, _ color: Color) -> String {
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
        colorToConsoleColor(color) +
        String(s[strMatchStartIndex ..< strMatchEndIndex]) +
        ConsoleColor.RESET +
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
                    formattedDirPath = colorize(formattedDirPath, match!.range.lowerBound, match!.range.upperBound, settings.dirColor)
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
                formattedFileName = colorize(formattedFileName, match!.range.lowerBound, match!.range.upperBound, settings.fileColor)
                break
            }
        }
        if !settings.inExtensions.isEmpty {
            let idx = formattedFileName.lastIndex(of: ".")
            if idx != nil {
                let utf16Offset = idx!.utf16Offset(in: formattedFileName)
                if utf16Offset > 0 && utf16Offset < formattedFileName.count - 1 {
                    formattedFileName = colorize(formattedFileName, utf16Offset + 1, formattedFileName.count, settings.extColor)
                }
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

public class FileResultSorter {
    let settings: FindSettings

    public init(settings: FindSettings) {
        self.settings = settings
    }

    public func cmpByFilePath(_ fr1: FileResult, _ fr2: FileResult) -> Int {
        let (fp1, fp2) = self.settings.sortCaseInsensitive
                         ? (fr1.filePath.lowercased(), fr2.filePath.lowercased())
                         : (fr1.filePath, fr2.filePath)
        let (p1, f1) = FileUtil.splitPath(fp1)
        let (p2, f2) = FileUtil.splitPath(fp2)
        if p1 == p2 {
            if f1 == f2 {
                return 0
            }
            return f1 < f2 ? -1 : 1
        }
        return p1 < p2 ? -1 : 1
    }

    public func sortByFilePath(_ fr1: FileResult, _ fr2: FileResult) -> Bool {
        return cmpByFilePath(fr1, fr2) <= 0
    }

    public func cmpByFileName(_ fr1: FileResult, _ fr2: FileResult) -> Int {
        let (fp1, fp2) = self.settings.sortCaseInsensitive
                         ? (fr1.filePath.lowercased(), fr2.filePath.lowercased())
                         : (fr1.filePath, fr2.filePath)
        let (p1, f1) = FileUtil.splitPath(fp1)
        let (p2, f2) = FileUtil.splitPath(fp2)
        if f1 == f2 {
            if p1 == p2 {
                return 0
            }
            return p1 < p2 ? -1 : 1
        }
        return f1 < f2 ? -1 : 1
    }

    public func sortByFileName(_ fr1: FileResult, _ fr2: FileResult) -> Bool {
        return cmpByFileName(fr1, fr2) <= 0
    }

    public func cmpByFileSize(_ fr1: FileResult, _ fr2: FileResult) -> Int {
        if fr1.fileSize == fr2.fileSize {
            return cmpByFilePath(fr1, fr2)
        }
        return fr1.fileSize < fr2.fileSize ? -1 : 1
    }

    public func sortByFileSize(_ fr1: FileResult, _ fr2: FileResult) -> Bool {
        return cmpByFileSize(fr1, fr2) <= 0
    }

    public func cmpByFileType(_ fr1: FileResult, _ fr2: FileResult) -> Int {
        if fr1.fileType == fr2.fileType {
            return cmpByFilePath(fr1, fr2)
        }
        return FileTypes.toName(fr1.fileType) < FileTypes.toName(fr2.fileType) ? -1 : 1
    }

    public func sortByFileType(_ fr1: FileResult, _ fr2: FileResult) -> Bool {
        return cmpByFileType(fr1, fr2) <= 0
    }

    public func cmpByLastMod(_ fr1: FileResult, _ fr2: FileResult) -> Int {
        if fr1.lastMod == nil || fr2.lastMod == nil || fr1.lastMod! == fr2.lastMod! {
            return cmpByFilePath(fr1, fr2)
        }
        return fr1.lastMod! < fr2.lastMod! ? -1 : 1
    }

    public func sortByLastMod(_ fr1: FileResult, _ fr2: FileResult) -> Bool {
        return cmpByLastMod(fr1, fr2) <= 0
    }

    private func getFileResultComparator() -> (FileResult, FileResult) -> Bool {
        if settings.sortDescending {
            switch settings.sortBy {
            case SortBy.fileName:
                return { (fr1: FileResult, fr2: FileResult) -> Bool in return self.sortByFileName(fr2, fr1) }
            case SortBy.fileSize:
                return { (fr1: FileResult, fr2: FileResult) -> Bool in return self.sortByFileSize(fr2, fr1) }
            case SortBy.fileType:
                return { (fr1: FileResult, fr2: FileResult) -> Bool in return self.sortByFileType(fr2, fr1) }
            case SortBy.lastMod:
                return { (fr1: FileResult, fr2: FileResult) -> Bool in return self.sortByLastMod(fr2, fr1) }
            default:
                return { (fr1: FileResult, fr2: FileResult) -> Bool in return self.sortByFilePath(fr2, fr1) }
            }
        } else {
            switch settings.sortBy {
            case SortBy.fileName:
                return sortByFileName
            case SortBy.fileSize:
                return sortByFileSize
            case SortBy.fileType:
                return sortByFileType
            case SortBy.lastMod:
                return sortByLastMod
            default:
                return sortByFilePath
            }
        }
    }

    public func sort(_ fileResults: [FileResult]) -> [FileResult] {
        let fileResultComparator = getFileResultComparator()
        return fileResults.sorted(by: fileResultComparator)
    }
}

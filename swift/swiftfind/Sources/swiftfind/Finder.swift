//
//  Finder.swift
//  swiftfind
//
//  Created by Cary Clark on 5/20/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
// FIXME: comparison operators with optionals were removed from the Swift Standard Libary.
// Consider refactoring the code to use the non-optional operators.
private func < <T: Comparable>(lhs: T?, rhs: T?) -> Bool {
    switch (lhs, rhs) {
    case let (l?, r?):
        l < r
    case (nil, _?):
        true
    default:
        false
    }
}

public class Finder {
    let fileTypes = FileTypes()
    let settings: FindSettings

    public init(settings: FindSettings) throws {
        self.settings = settings
        try validateSettings()
    }

    private func validateSettings() throws {
        if settings.paths.isEmpty {
            throw FindError(msg: "Startpath not defined")
        } else if !settings.paths.allSatisfy({ FileUtil.exists($0) }) {
            throw FindError(msg: "Startpath not found")
        } else if !settings.paths.allSatisfy({ FileUtil.isReadableFile($0) }) {
            throw FindError(msg: "Startpath not readable")
        } else if settings.maxDepth > -1, settings.maxDepth < settings.minDepth {
            throw FindError(msg: "Invalid range for mindepth and maxdepth")
        } else if settings.maxLastMod != nil, settings.minLastMod != nil, settings.maxLastMod! < settings.minLastMod! {
            throw FindError(msg: "Invalid range for minlastmod and maxlastmod")
        } else if settings.maxSize > 0, settings.maxSize < settings.minSize {
            throw FindError(msg: "Invalid range for minsize and maxsize")
        }
    }

    private func matchesAnyPattern(_ str: String, _ patterns: [Regex]) -> Bool {
        patterns.filter { $0.test(str) }.count > 0
    }

    private func anyMatchesAnyPattern(_ strs: [String], _ patterns: [Regex]) -> Bool {
        strs.filter { self.matchesAnyPattern($0, patterns) }.count > 0
    }

    private func filterByExtensions(_ ext: String, inExtensions: Set<String>,
                                    outExtensions: Set<String>) -> Bool {
        (inExtensions.isEmpty || inExtensions.contains(ext))
            && (outExtensions.isEmpty || !outExtensions.contains(ext))
    }

    private func filterByPatterns(_ str: String, inPatterns: [Regex],
                                  outPatterns: [Regex]) -> Bool {
        (inPatterns.isEmpty || matchesAnyPattern(str, inPatterns))
            && (outPatterns.isEmpty || !matchesAnyPattern(str, outPatterns))
    }

    private func filterByFileTypes(_ fileType: FileType, inFileTypes: [FileType],
                                   outFileTypes: [FileType]) -> Bool {
        (inFileTypes.isEmpty || inFileTypes.contains(fileType))
            && (outFileTypes.isEmpty || !outFileTypes.contains(fileType))
    }

    private func filterByStat(_ stat: [FileAttributeKey: Any]?) -> Bool {
        if stat != nil {
            if settings.maxLastMod != nil || settings.minLastMod != nil {
                let lastMod: Date = stat?[FileAttributeKey.modificationDate] as! Date
                if (settings.maxLastMod != nil && lastMod > settings.maxLastMod!)
                    || (settings.minLastMod != nil && lastMod < settings.minLastMod!) {
                    return false
                }
            }

            if settings.maxSize > 0 || settings.minSize > 0 {
                let fileSize: UInt64 = stat?[FileAttributeKey.size] as! UInt64
                if (settings.maxSize > 0 && fileSize > settings.maxSize)
                    || (settings.minSize > 0 && fileSize < settings.minSize) {
                    return false
                }
            }
        }
        return true
    }

    public func isMatchingDir(_ dirPath: String) -> Bool {
        if !settings.includeHidden, FileUtil.isHidden(dirPath) {
            return false
        }
        return filterByPatterns(dirPath, inPatterns: settings.inDirPatterns,
                                outPatterns: settings.outDirPatterns)
    }

    public func isMatchingFile(_ fileName: String) -> Bool {
        isMatchingFile(fileName, fileType: fileTypes.getFileType(fileName))
    }

    public func isMatchingFile(_ fileName: String, fileType: FileType) -> Bool {
        if !settings.includeHidden, FileUtil.isHiddenFile(fileName) {
            return false
        }
        if !settings.inExtensions.isEmpty || !settings.outExtensions.isEmpty {
            let filteredByExtensions = filterByExtensions(FileUtil.getExtension(fileName),
                                                          inExtensions: settings.inExtensions,
                                                          outExtensions: settings.outExtensions)
            if !filteredByExtensions {
                return false
            }
        }
        return filterByPatterns(fileName,
                                inPatterns: settings.inFilePatterns,
                                outPatterns: settings.outFilePatterns)
            && filterByFileTypes(fileType,
                                 inFileTypes: settings.inFileTypes,
                                 outFileTypes: settings.outFileTypes)
    }

    public func isMatchingFileResult(_ fileResult: FileResult) -> Bool {
        let fileName = URL(fileURLWithPath: fileResult.filePath).lastPathComponent
        if !settings.includeHidden, FileUtil.isHiddenFile(fileName) {
            return false
        }
        if !settings.inExtensions.isEmpty || !settings.outExtensions.isEmpty {
            let filteredByExtensions = filterByExtensions(FileUtil.getExtension(fileName),
                                                          inExtensions: settings.inExtensions,
                                                          outExtensions: settings.outExtensions)
            if !filteredByExtensions {
                return false
            }
        }
        return (filterByPatterns(fileName,
                                 inPatterns: settings.inFilePatterns,
                                 outPatterns: settings.outFilePatterns)
                && filterByFileTypes(fileResult.fileType,
                                     inFileTypes: settings.inFileTypes,
                                     outFileTypes: settings.outFileTypes)
                && filterByStat(fileResult.stat))
    }

    public func isMatchingArchiveFile(_ fileName: String) -> Bool {
        if !settings.includeHidden, FileUtil.isHidden(fileName) {
            return false
        }
        if !settings.inArchiveExtensions.isEmpty || !settings.outArchiveExtensions.isEmpty {
            let filteredByExtensions = filterByExtensions(FileUtil.getExtension(fileName),
                                                          inExtensions: settings.inArchiveExtensions,
                                                          outExtensions: settings.outArchiveExtensions)
            if !filteredByExtensions {
                return false
            }
        }
        return filterByPatterns(fileName, inPatterns: settings.inArchiveFilePatterns,
                                outPatterns: settings.outArchiveFilePatterns)
    }

    public func isMatchingArchiveFileResult(_ fileResult: FileResult) -> Bool {
        let fileName = URL(fileURLWithPath: fileResult.filePath).lastPathComponent
        if !settings.includeHidden, FileUtil.isHidden(fileName) {
            return false
        }
        if !settings.inArchiveExtensions.isEmpty || !settings.outArchiveExtensions.isEmpty {
            let filteredByExtensions = filterByExtensions(FileUtil.getExtension(fileName),
                                                          inExtensions: settings.inArchiveExtensions,
                                                          outExtensions: settings.outArchiveExtensions)
            if !filteredByExtensions {
                return false
            }
        }
        return filterByPatterns(fileName, inPatterns: settings.inArchiveFilePatterns,
                                outPatterns: settings.outArchiveFilePatterns)
    }

    public func filterToFileResult(_ filePath: String) -> FileResult? {
        let fileName = URL(fileURLWithPath: filePath).lastPathComponent
        if !settings.includeHidden, FileUtil.isHidden(fileName) {
            return nil
        }
        let fileType = fileTypes.getFileType(fileName)
        var stat: [FileAttributeKey: Any]?
        if self.settings.needStat() {
            do {
                stat = try FileManager.default.attributesOfItem(atPath: filePath)
            } catch {
                print("Error: \(error)")
            }
        }
        let fr = FileResult(filePath: filePath, fileType: fileType, stat: stat)
        if fileType == FileType.archive {
            if settings.includeArchives && isMatchingArchiveFileResult(fr) {
                return fr
            }
            return nil
        }
        if !settings.archivesOnly && isMatchingFileResult(fr) {
            return fr
        }
        return nil
    }

    // gets all FileResults recursively
    private func getFileResults(_ filePath: String) -> [FileResult] {
        var fileResults = [FileResult]()
        if let enumerator = FileUtil.enumerator(forPath: filePath, settings: settings) {
            for case let fileURL as URL in enumerator {
                do {
                    let fileAttributes = try fileURL.resourceValues(forKeys: [.isDirectoryKey, .isRegularFileKey])
                    if fileAttributes.isDirectory! {
                        if (settings.maxDepth > 0 && enumerator.level > settings.maxDepth)
                            || !isMatchingDir(fileURL.path) {
                            enumerator.skipDescendants()
                        }
                    } else if fileAttributes.isRegularFile! {
                        if enumerator.level >= settings.minDepth, settings.maxDepth < 1
                            || enumerator.level <= settings.maxDepth {
                            if let fileResult = filterToFileResult(fileURL.path) {
                                fileResults.append(fileResult)
                            }
                        }
                    }
                } catch { print(error, fileURL) }
            }
        }
        return fileResults
    }

    private func getSortByFilePath() -> (FileResult, FileResult) -> Bool {
        { (fr1: FileResult, fr2: FileResult) -> Bool in
            let (p1, f1) = FileUtil.splitPath(self.settings.sortCaseInsensitive
                                              ? fr1.filePath.lowercased()
                                              : fr1.filePath)
            let (p2, f2) = FileUtil.splitPath(self.settings.sortCaseInsensitive
                                              ? fr2.filePath.lowercased()
                                              : fr2.filePath)
            if p1 == p2 {
                return f1 < f2
            }
            return p1 < p2
        }
    }

    private func getSortByFileName() -> (FileResult, FileResult) -> Bool {
        { (fr1: FileResult, fr2: FileResult) -> Bool in
            let (p1, f1) = FileUtil.splitPath(self.settings.sortCaseInsensitive
                                              ? fr1.filePath.lowercased()
                                              : fr1.filePath)
            let (p2, f2) = FileUtil.splitPath(self.settings.sortCaseInsensitive
                                              ? fr2.filePath.lowercased()
                                              : fr2.filePath)
            if f1 == f2 {
                return p1 < p2
            }
            return f1 < f2
        }
    }

    private func getSortByFileSize() -> (FileResult, FileResult) -> Bool {
        { (fr1: FileResult, fr2: FileResult) -> Bool in
            let fr1Size: UInt64 = fr1.stat?[FileAttributeKey.size] as! UInt64
            let fr2Size: UInt64 = fr2.stat?[FileAttributeKey.size] as! UInt64

            if fr1Size == fr2Size {
                let sortByFilePath = self.getSortByFilePath()
                return sortByFilePath(fr1, fr2)
            }
            return fr1Size < fr2Size
        }
    }

    private func getSortByFileType() -> (FileResult, FileResult) -> Bool {
        { (fr1: FileResult, fr2: FileResult) -> Bool in
            if fr1.fileType == fr2.fileType {
                let sortByFilePath = self.getSortByFilePath()
                return sortByFilePath(fr1, fr2)
            }
            return FileTypes.toName(fr1.fileType) < FileTypes.toName(fr2.fileType)
        }
    }

    private func getSortByLastMod() -> (FileResult, FileResult) -> Bool {
        { (fr1: FileResult, fr2: FileResult) -> Bool in
            let fr1LastMod: Date = fr1.stat?[FileAttributeKey.modificationDate] as! Date
            let fr2LastMod: Date = fr2.stat?[FileAttributeKey.modificationDate] as! Date

            if fr1LastMod == fr2LastMod {
                let sortByFilePath = self.getSortByFilePath()
                return sortByFilePath(fr1, fr2)
            }
            return fr1LastMod < fr2LastMod
        }
    }

    public func sortFileResults(_ fileResults: [FileResult]) -> [FileResult] {
        var sortedFileResults = [FileResult]()
        switch settings.sortBy {
        case SortBy.fileName:
            sortedFileResults = fileResults.sorted(by: getSortByFileName())
        case SortBy.fileSize:
            sortedFileResults = fileResults.sorted(by: getSortByFileSize())
        case SortBy.fileType:
            sortedFileResults = fileResults.sorted(by: getSortByFileType())
        case SortBy.lastMod:
            sortedFileResults = fileResults.sorted(by: getSortByLastMod())
        default:
            sortedFileResults = fileResults.sorted(by: getSortByFilePath())
        }

        if settings.sortDescending {
            sortedFileResults.reverse()
        }

        return sortedFileResults
    }

    public func find() -> [FileResult] {
        var fileResults = [FileResult]()
        for p in settings.paths {
            if FileUtil.isDirectory(p) {
                // if maxDepth is zero, we can skip since a directory cannot be a result
                if settings.maxDepth != 0 {
                    let pFiles: [FileResult] = getFileResults(p)
                    fileResults.append(contentsOf: pFiles)
                }
            } else {
                // if minDepth > zero, we can skip since the file is at depth zero
                if settings.minDepth <= 0 {
                    if let fileResult = filterToFileResult(p) {
                        fileResults.append(fileResult)
                    }
                }
            }
        }
        return sortFileResults(fileResults)
    }
}

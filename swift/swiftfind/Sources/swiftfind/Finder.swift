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
        return l < r
    case (nil, _?):
        return true
    default:
        return false
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
        }
    }

    private func matchesAnyPattern(_ str: String, _ patterns: [Regex]) -> Bool {
        patterns.filter { $0.test(str) }.count > 0
    }

    private func anyMatchesAnyPattern(_ strs: [String], _ patterns: [Regex]) -> Bool {
        strs.filter { self.matchesAnyPattern($0, patterns) }.count > 0
    }

    private func filterByExtensions(_ ext: String, inExtensions: Set<String>,
                                    outExtensions: Set<String>) -> Bool
    {
        ((inExtensions.isEmpty || inExtensions.contains(ext))
            && (outExtensions.isEmpty || !outExtensions.contains(ext)))
    }

    private func filterByPatterns(_ str: String, inPatterns: [Regex],
                                  outPatterns: [Regex]) -> Bool
    {
        ((inPatterns.isEmpty || matchesAnyPattern(str, Array(inPatterns)))
            && (outPatterns.isEmpty || !matchesAnyPattern(str, Array(outPatterns))))
    }

    private func filterByFileTypes(_ fileType: FileType, inFileTypes: [FileType],
                                   outFileTypes: [FileType]) -> Bool
    {
        ((inFileTypes.isEmpty || inFileTypes.contains(fileType))
            && (outFileTypes.isEmpty || !outFileTypes.contains(fileType)))
    }

    public func isMatchingDir(_ dirPath: String) -> Bool {
        if FileUtil.isHidden(dirPath), settings.excludeHidden {
            return false
        }
        return filterByPatterns(dirPath, inPatterns: settings.inDirPatterns,
                                outPatterns: settings.outDirPatterns)
    }

    public func isMatchingFile(_ fileName: String) -> Bool {
        isMatchingFile(fileName, fileType: fileTypes.getFileType(fileName))
    }

    public func isMatchingFile(_ fileName: String, fileType: FileType) -> Bool {
        if settings.excludeHidden, FileUtil.isHiddenFile(fileName) {
            return false
        }
        return (filterByExtensions(FileUtil.getExtension(fileName),
                                   inExtensions: settings.inExtensions,
                                   outExtensions: settings.outExtensions)
                && filterByPatterns(fileName,
                                    inPatterns: settings.inFilePatterns,
                                    outPatterns: settings.outFilePatterns)
                && filterByFileTypes(fileType,
                                     inFileTypes: settings.inFileTypes,
                                     outFileTypes: settings.outFileTypes))
    }

    public func isMatchingArchiveFile(_ fileName: String) -> Bool {
        if settings.excludeHidden, FileUtil.isHidden(fileName) {
            return false
        }
        return (filterByExtensions(FileUtil.getExtension(fileName),
                                   inExtensions: settings.inArchiveExtensions,
                                   outExtensions: settings.outArchiveExtensions)
                && filterByPatterns(fileName, inPatterns: settings.inArchiveFilePatterns,
                                    outPatterns: settings.outArchiveFilePatterns))
    }

    public func filterToFileResult(_ filePath: String) -> FileResult? {
        let fileName = URL(fileURLWithPath: filePath).lastPathComponent
        if settings.excludeHidden, FileUtil.isHidden(fileName) {
            return nil
        }
        let fileType = fileTypes.getFileType(filePath)
        let fr = FileResult(filePath: filePath, fileType: fileType)
        if (fileType == FileType.archive) {
            if (settings.includeArchives && isMatchingArchiveFile(fileName)) {
                return fr
            }
            return nil
        }
        if (!settings.archivesOnly && isMatchingFile(fileName, fileType: fileType)) {
            return fr
        }
        return nil
    }

    // gets all FileResults recursively
    private func getFileResults(_ filePath: String) -> [FileResult] {
        var findFiles = [FileResult]()
        if let enumerator = FileUtil.enumerator(forPath: filePath, settings: settings) {
            for case let fileURL as URL in enumerator {
                do {
                    let fileAttributes = try fileURL.resourceValues(forKeys: [.isDirectoryKey, .isRegularFileKey])
                    if fileAttributes.isDirectory! {
                        if !isMatchingDir(fileURL.path) {
                            enumerator.skipDescendants()
                        }
                    } else if fileAttributes.isRegularFile! {
                        if let findFile = filterToFileResult(fileURL.path) {
                            findFiles.append(findFile)
                        }
                    }
                } catch { print(error, fileURL) }
            }
        }
        return findFiles
    }

    public func find() -> [FileResult] {
        var fileResults = [FileResult]()
        for p in settings.paths {
            if FileUtil.isDirectory(p) {
                let pFiles: [FileResult] = getFileResults(p)
                fileResults.append(contentsOf: pFiles)
            } else {
                let fileType = fileTypes.getFileType(p)
                fileResults.append(FileResult(filePath: p, fileType: fileType))
            }
        }
        return fileResults
    }
}

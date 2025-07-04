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

public let INVALID_RANGE_MINDEPTH_MAXDEPTH = "Invalid range for mindepth and maxdepth"
public let INVALID_RANGE_MINLASTMOD_MAXLASTMOD = "Invalid range for minlastmod and maxlastmod"
public let INVALID_RANGE_MINSIZE_MAXSIZE = "Invalid range for minsize and maxsize"
public let STARTPATH_NOT_DEFINED = "Startpath not defined"
public let STARTPATH_NOT_FOUND = "Startpath not found"
public let STARTPATH_NOT_READABLE = "Startpath not readable"
public let STARTPATH_NOT_MATCH_FIND_SETTINGS = "Startpath does not match find settings"

public class Finder {
    let fileTypes = FileTypes()
    let settings: FindSettings

    public init(settings: FindSettings) throws {
        self.settings = settings
        try validateSettings()
    }

    private func validateSettings() throws {
        if settings.paths.isEmpty {
            throw FindError(msg: STARTPATH_NOT_DEFINED)
        } else if !settings.paths.allSatisfy({ FileUtil.exists($0) || FileUtil.exists(FileUtil.expandPath($0)) }) {
            throw FindError(msg: STARTPATH_NOT_FOUND)
        } else if !settings.paths.allSatisfy({ FileUtil.isReadableFile($0) || FileUtil.isReadableFile(FileUtil.expandPath($0)) }) {
            throw FindError(msg: STARTPATH_NOT_READABLE)
        } else if settings.maxDepth > -1, settings.maxDepth < settings.minDepth {
            throw FindError(msg: INVALID_RANGE_MINDEPTH_MAXDEPTH)
        } else if settings.maxLastMod != nil, settings.minLastMod != nil, settings.maxLastMod! < settings.minLastMod! {
            throw FindError(msg: INVALID_RANGE_MINLASTMOD_MAXLASTMOD)
        } else if settings.maxSize > 0, settings.maxSize < settings.minSize {
            throw FindError(msg: INVALID_RANGE_MINSIZE_MAXSIZE)
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

    private func filterBySize(_ fileSize: UInt64, maxSize: UInt64, minSize: UInt64) -> Bool {
        (maxSize == 0 || fileSize <= maxSize) &&
        (minSize == 0 || fileSize >= minSize)
    }

    private func filterByLastMod(_ lastMod: Date?, maxLastMod: Date?, minLastMod: Date?) -> Bool {
        (maxLastMod == nil || lastMod! <= maxLastMod!) &&
        (minLastMod == nil || lastMod! >= minLastMod!)
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
                && filterBySize(fileResult.fileSize, maxSize: settings.maxSize, minSize: settings.minSize)
                && filterByLastMod(fileResult.lastMod, maxLastMod: settings.maxLastMod, minLastMod: settings.minLastMod))
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
        var fileSize: UInt64 = 0
        var lastMod: Date? = nil
        if self.settings.needSize() || self.settings.needLastMod() {
            do {
                let stat: [FileAttributeKey: Any] = try FileUtil.getFileAttributes(filePath)
                if self.settings.needSize() { fileSize = stat[FileAttributeKey.size] as! UInt64 }
                if self.settings.needLastMod() { lastMod = stat[FileAttributeKey.modificationDate] as? Date }

            } catch {
                print("Error: \(error)")
            }
        }
        let fr = FileResult(filePath: filePath, fileType: fileType, fileSize: fileSize, lastMod: lastMod)
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
    private func recGetFileResults(_ dirPath: String, minDepth: Int32, maxDepth: Int32, currentDepth: Int32) -> [FileResult] {
        var fileResults = [FileResult]()
        var recurse: Bool = true
        if currentDepth == maxDepth {
            recurse = false
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return fileResults
        }

        let pathElems = try! FileManager.default.contentsOfDirectory(atPath: dirPath)
        var pathDirs = [String]()

        for pathElem in pathElems {
            let path = FileUtil.joinPath(dirPath, childPath: pathElem)
            var linkIsDir: Bool = false
            var linkIsFile: Bool = false
            if FileUtil.isSymlink(path) {
                if settings.followSymlinks {
                    // TODO: determine if dir or file
                    if let resolvedPath = FileUtil.getSymlinkTarget(path) {
                        if FileUtil.isDirectory(resolvedPath) {
                            linkIsDir = true
                        } else if FileUtil.isReadableFile(path) {
                            linkIsFile = true
                        }
                    }
                } else {
                    continue
                }
            }
            if FileUtil.isDirectory(path) || linkIsDir {
                if recurse && isMatchingDir(pathElem) {
                    pathDirs.append(path)
                }
            } else if (FileUtil.isReadableFile(path) || linkIsFile) && (minDepth < 0 || currentDepth >= minDepth) {
                let fileResult = filterToFileResult(path)
                if fileResult != nil {
                    fileResults.append(fileResult!)
                }
            }
        }

        for pathDir in pathDirs {
            let pathResults = recGetFileResults(pathDir, minDepth: minDepth, maxDepth: maxDepth, currentDepth: (currentDepth + 1))
            fileResults.append(contentsOf: pathResults)
        }

        return fileResults
    }

    // gets all FileResults recursively
    private func getFileResults(_ filePath: String) throws -> [FileResult] {
        var fp = filePath
        if !FileUtil.exists(filePath) {
            fp = FileUtil.expandPath(filePath)
        }
        var fileResults = [FileResult]()
        if FileUtil.isDirectory(fp) {
            if settings.maxDepth == 0 {
                return fileResults
            }
            if self.isMatchingDir(fp) {
                let maxDepth = settings.recursive ? settings.maxDepth : 1
                let pathResults = recGetFileResults(fp, minDepth: settings.minDepth, maxDepth: maxDepth, currentDepth: 1)
                fileResults.append(contentsOf: pathResults)
            } else {
                throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
            }
        } else {
            // if minDepth > zero, we can skip since the file is at depth zero
            if settings.minDepth > 0 {
                return fileResults
            }
            if let fileResult = filterToFileResult(fp) {
                fileResults.append(fileResult)
            } else {
                throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
            }
        }
        return fileResults
    }

    public func find() throws -> [FileResult] {
        var fileResults = [FileResult]()
        for p in settings.paths {
            let pathResults: [FileResult] = try getFileResults(p)
            fileResults.append(contentsOf: pathResults)
        }
        let fileResultSorter = FileResultSorter(settings: settings)
        return fileResultSorter.sort(fileResults)
    }

    func getMatchingDirs(_ fileResults: [FileResult]) -> [String] {
        fileResults.map {
            URL(fileURLWithPath: $0.filePath).deletingLastPathComponent().path
        }.sorted().unique()
    }

    public func printMatchingDirs(_ fileResults: [FileResult], _ formatter: FileResultFormatter) -> Void {
        let dirs = getMatchingDirs(fileResults)
        if dirs.isEmpty {
            logMsg("\nMatching directories: 0")
        } else {
            logMsg("\nMatching directories (\(dirs.count)):")
            for dir in dirs {
                logMsg(formatter.formatDirPath(dir))
            }
        }
    }

    func getMatchingFiles(_ fileResults: [FileResult]) -> [String] {
        fileResults.map(\.filePath)
    }

    public func printMatchingFiles(_ fileResults: [FileResult], _ formatter: FileResultFormatter) -> Void {
        if fileResults.isEmpty {
            logMsg("\nMatching files: 0")
        } else {
            logMsg("\nMatching files (\(fileResults.count)):")
            for fileResult in fileResults {
                logMsg(formatter.formatFileResult(fileResult))
            }
        }
    }
}

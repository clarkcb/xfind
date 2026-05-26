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
        }
        for path in settings.paths {
            var p = path
            if (!FileUtil.exists(p)) {
                p = FileUtil.expandPath(p)
                if (!FileUtil.exists(p)) {
                    throw FindError(msg: STARTPATH_NOT_FOUND)
                }
            }
            if (!FileUtil.isReadableFile(p)) {
                throw FindError(msg: STARTPATH_NOT_READABLE)
            }

            var resolvedPath: String? = p
            if FileUtil.isSymlink(p) {
                if settings.followSymlinks {
                    resolvedPath = FileUtil.getSymlinkTarget(p)
                } else {
                    throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
                }
            }
            if FileUtil.isDirectory(resolvedPath!) {
                // still check p and not resolvedPath because p is the name entered
                if !isTraversableDirPath(p) {
                    throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
                }
            } else if FileUtil.isReadableFile(resolvedPath!) {
                // still check p and not resolvedPath because p is the name entered
                if filterToFileResult(p) == nil {
                    throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
                }
            } else {
                // TODO: start path is unknown/invalid type
                throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
            }
        }
        if settings.maxDepth > -1, settings.maxDepth < settings.minDepth {
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

    private func emptyOrMatchesAnyPattern(_ str: String, _ patterns: [Regex]) -> Bool {
        patterns.isEmpty || matchesAnyPattern(str, patterns)
    }

    private func emptyOrNotMatchesAnyPattern(_ str: String, _ patterns: [Regex]) -> Bool {
        patterns.isEmpty || !matchesAnyPattern(str, patterns)
    }

    private func emptyOrAnyMatchesAnyPattern(_ strs: [String], _ patterns: [Regex]) -> Bool {
        patterns.isEmpty || anyMatchesAnyPattern(strs, patterns)
    }

    private func emptyOrNotAnyMatchesAnyPattern(_ strs: [String], _ patterns: [Regex]) -> Bool {
        patterns.isEmpty || !anyMatchesAnyPattern(strs, patterns)
    }

    private func emptyOrMatchesAnyString(_ str: String, _ strs: Set<String>) -> Bool {
        strs.isEmpty || strs.contains(str)
    }

    private func emptyOrNotMatchesAnyString(_ str: String, _ strs: Set<String>) -> Bool {
        strs.isEmpty || !strs.contains(str)
    }

    private func emptyOrMatchesAnyFileType(_ fileType: FileType, _ fileTypes: [FileType]) -> Bool {
        fileTypes.isEmpty || fileTypes.contains(fileType)
    }

    private func emptyOrNotMatchesAnyFileType(_ fileType: FileType, _ fileTypes: [FileType]) -> Bool {
        fileTypes.isEmpty || !fileTypes.contains(fileType)
    }

    public func isMatchingDirPathByHidden(_ dirPath: String) -> Bool {
        settings.includeHidden || !FileUtil.isHiddenPath(dirPath)
    }

    public func isMatchingDirPathByInPatterns(_ dirPath: String) -> Bool {
        emptyOrAnyMatchesAnyPattern(FileUtil.getPathComponents(dirPath), settings.inDirPatterns)
    }

    public func isMatchingDirPathByOutPatterns(_ dirPath: String) -> Bool {
        emptyOrNotAnyMatchesAnyPattern(FileUtil.getPathComponents(dirPath), settings.outDirPatterns)
    }

    public func isTraversableDirPath(_ dirPath: String) -> Bool {
        isMatchingDirPathByHidden(dirPath)
        && isMatchingDirPathByOutPatterns(dirPath)
    }

    public func isMatchingDirPath(_ dirPath: String) -> Bool {
        isMatchingDirPathByHidden(dirPath)
        && isMatchingDirPathByInPatterns(dirPath)
        && isMatchingDirPathByOutPatterns(dirPath)
    }

    public func isNullOrMatchingDirPath(_ dirPath: String?) -> Bool {
        dirPath == nil
        || (isMatchingDirPathByHidden(dirPath!)
        && isMatchingDirPathByInPatterns(dirPath!)
        && isMatchingDirPathByOutPatterns(dirPath!))
    }

    public func isMatchingFileNameByHidden(_ fileName: String) -> Bool {
        settings.includeHidden || !FileUtil.isHiddenName(fileName)
    }
    
    public func isMatchingArchiveExtension(_ ext: String) -> Bool {
        emptyOrMatchesAnyString(ext, settings.inArchiveExtensions)
        && emptyOrNotMatchesAnyString(ext, settings.outArchiveExtensions)
    }

    public func isMatchingArchiveExtensionForFilePath(_ filePath: String) -> Bool {
        if !settings.inArchiveExtensions.isEmpty || !settings.outArchiveExtensions.isEmpty {
            let ext = FileUtil.getExtension(filePath)
            return isMatchingArchiveExtension(ext)
        }
        return true
    }

    public func isMatchingArchiveFileName(_ fileName: String) -> Bool {
        emptyOrMatchesAnyPattern(fileName, settings.inArchiveFilePatterns)
        && emptyOrNotMatchesAnyPattern(fileName, settings.outArchiveFilePatterns)
    }

    public func isMatchingArchiveFileNameForFilePath(_ filePath: String) -> Bool {
        if !settings.inArchiveFilePatterns.isEmpty || !settings.outArchiveFilePatterns.isEmpty {
            let fileName = URL(fileURLWithPath: filePath).lastPathComponent
            return isMatchingArchiveFileName(fileName)
        }
        return true
    }

    public func isMatchingArchiveFilePath(_ filePath: String) -> Bool {
        isMatchingArchiveExtensionForFilePath(filePath)
        && isMatchingArchiveFileNameForFilePath(filePath)
    }

    public func isMatchingArchiveFileResult(_ fileResult: FileResult) -> Bool {
        isMatchingArchiveFilePath(fileResult.filePath)
    }


    public func isMatchingExtension(_ ext: String) -> Bool {
        emptyOrMatchesAnyString(ext, settings.inExtensions)
        && emptyOrNotMatchesAnyString(ext, settings.outExtensions)
    }

    public func isMatchingExtensionForFilePath(_ filePath: String) -> Bool {
        if !settings.inExtensions.isEmpty || !settings.outExtensions.isEmpty {
            let ext = FileUtil.getExtension(filePath)
            return isMatchingExtension(ext)
        }
        return true
    }

    public func isMatchingFileName(_ fileName: String) -> Bool {
        emptyOrMatchesAnyPattern(fileName, settings.inFilePatterns)
        && emptyOrNotMatchesAnyPattern(fileName, settings.outFilePatterns)
    }

    public func isMatchingFileNameForFilePath(_ filePath: String) -> Bool {
        if !settings.inFilePatterns.isEmpty || !settings.outFilePatterns.isEmpty {
            let fileName = URL(fileURLWithPath: filePath).lastPathComponent
            return isMatchingFileName(fileName)
        }
        return true
    }

    public func isMatchingFilePath(_ filePath: String) -> Bool {
        isMatchingExtensionForFilePath(filePath)
        && isMatchingFileNameForFilePath(filePath)
    }

    public func isMatchingFileType(_ fileType: FileType) -> Bool {
        emptyOrMatchesAnyFileType(fileType, settings.inFileTypes)
        && emptyOrNotMatchesAnyFileType(fileType, settings.outFileTypes)
    }

    public func isMatchingFileSize(_ fileSize: UInt64) -> Bool {
        (settings.maxSize <= 0 || fileSize <= settings.maxSize) &&
        (settings.minSize <= 0 || fileSize >= settings.minSize)
    }

    public func isMatchingLastMod(_ lastMod: Date?) -> Bool {
        (settings.maxLastMod == nil || lastMod! <= settings.maxLastMod!) &&
        (settings.minLastMod == nil || lastMod! >= settings.minLastMod!)
    }

    public func isMatchingFileResult(_ fileResult: FileResult) -> Bool {
        isMatchingFilePath(fileResult.filePath)
        && isMatchingFileType(fileResult.fileType)
        && isMatchingFileSize(fileResult.fileSize)
        && isMatchingLastMod(fileResult.lastMod)
    }

    public func filterArchiveFilePathToFileResult(_ filePath: String) -> FileResult? {
        if !settings.includeArchives && !settings.archivesOnly {
            return nil
        }

        if (!isMatchingArchiveFilePath(filePath)) {
            return nil
        }

        let fileSize: UInt64 = 0
        let lastMod: Date? = nil

        return FileResult(filePath: filePath, fileType: FileType.archive, fileSize: fileSize, lastMod: lastMod)
    }

    public func filterRegularFilePathToFileResult(_ filePath: String, _ fileType: FileType) -> FileResult? {
        if settings.archivesOnly {
            return nil
        }

        if !isMatchingFilePath(filePath) || !isMatchingFileType(fileType) {
            return nil
        }

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

        if !isMatchingFileSize(fileSize) || !isMatchingLastMod(lastMod) {
            return nil
        }

        return FileResult(filePath: filePath, fileType: fileType, fileSize: fileSize, lastMod: lastMod)
    }

    public func filterToFileResult(_ filePath: String) -> FileResult? {
        let (parent, fileName) = FileUtil.splitPath(filePath)
        if !isNullOrMatchingDirPath(parent) || !isMatchingFileNameByHidden(fileName) {
            return nil
        }

        let fileType = fileTypes.getFileType(fileName)
        if fileType == FileType.archive {
            return filterArchiveFilePathToFileResult(filePath)
        }
        return filterRegularFilePathToFileResult(filePath, fileType)
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

        let dirElems = try! FileManager.default.contentsOfDirectory(atPath: dirPath)
        var subDirs = [String]()

        for dirElem in dirElems {
            let subPath = FileUtil.joinPath(dirPath, childPath: dirElem)
            var linkIsDir: Bool = false
            var linkIsFile: Bool = false
            if FileUtil.isSymlink(subPath) {
                if settings.followSymlinks {
                    // Determine if dir or file
                    if let resolvedPath = FileUtil.getSymlinkTarget(subPath) {
                        if FileUtil.isDirectory(resolvedPath) {
                            linkIsDir = true
                        } else if FileUtil.isReadableFile(subPath) {
                            linkIsFile = true
                        }
                    }
                } else {
                    continue
                }
            }
            if FileUtil.isDirectory(subPath) || linkIsDir {
                if recurse && isTraversableDirPath(dirElem) {
                    subDirs.append(subPath)
                }
            } else if (FileUtil.isReadableFile(subPath) || linkIsFile) && (minDepth < 0 || currentDepth >= minDepth) {
                let fileResult = filterToFileResult(subPath)
                if fileResult != nil {
                    fileResults.append(fileResult!)
                }
            }
        }

        for subDir in subDirs {
            let dirResults = recGetFileResults(subDir, minDepth: minDepth, maxDepth: maxDepth, currentDepth: (currentDepth + 1))
            fileResults.append(contentsOf: dirResults)
        }

        return fileResults
    }

    // gets all FileResults recursively
    private func getFileResults(_ path: String) throws -> [FileResult] {
        var p = path
        if !FileUtil.exists(path) {
            p = FileUtil.expandPath(path)
            if !FileUtil.exists(p) {
                throw FindError(msg: STARTPATH_NOT_FOUND)
            }
        }
        var fileResults = [FileResult]()
        var linkIsDir: Bool = false
        var linkIsFile: Bool = false
        if FileUtil.isSymlink(p) {
            if settings.followSymlinks {
                // Determine if dir or file
                if let resolvedPath = FileUtil.getSymlinkTarget(p) {
                    if FileUtil.isDirectory(resolvedPath) {
                        linkIsDir = true
                    } else if FileUtil.isReadableFile(p) {
                        linkIsFile = true
                    }
                }
            } else {
                throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
            }
        }
        if FileUtil.isDirectory(p) || linkIsDir {
            // if maxDepth is zero, we can skip since a directory cannot be a result
            if settings.maxDepth == 0 {
                return fileResults
            }
            if self.isTraversableDirPath(p) {
                let maxDepth = settings.recursive ? settings.maxDepth : 1
                let pathResults = recGetFileResults(p, minDepth: settings.minDepth, maxDepth: maxDepth, currentDepth: 1)
                fileResults.append(contentsOf: pathResults)
            } else {
                throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
            }
        } else if FileUtil.isReadableFile(p) || linkIsFile {
            // if minDepth > zero, we can skip since the file is at depth zero
            if settings.minDepth > 0 {
                return fileResults
            }
            if let fileResult = filterToFileResult(p) {
                fileResults.append(fileResult)
            } else {
                throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
            }
        } else {
            throw FindError(msg: STARTPATH_NOT_MATCH_FIND_SETTINGS)
        }
        return fileResults
    }

    public func find() throws -> [FileResult] {
        var fileResults = [FileResult]()
        for p in settings.paths {
            let pathResults: [FileResult] = try getFileResults(p)
            fileResults.append(contentsOf: pathResults)
        }
        if fileResults.count > 1 {
            let fileResultSorter = FileResultSorter(settings: settings)
            return fileResultSorter.sort(fileResults)
        }
        return fileResults
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

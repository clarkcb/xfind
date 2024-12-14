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
        } else if !settings.paths.allSatisfy({ FileUtil.exists($0) || FileUtil.exists(FileUtil.expandPath($0)) }) {
            throw FindError(msg: "Startpath not found")
        } else if !settings.paths.allSatisfy({ FileUtil.isReadableFile($0) || FileUtil.isReadableFile(FileUtil.expandPath($0)) }) {
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
                let stat: [FileAttributeKey: Any] = try FileManager.default.attributesOfItem(atPath: filePath)
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
                throw FindError(msg: "Startpath does not match find settings")
            }
        } else {
            // if minDepth > zero, we can skip since the file is at depth zero
            if settings.minDepth > 0 {
                return fileResults
            }
            if let fileResult = filterToFileResult(fp) {
                fileResults.append(fileResult)
            } else {
                throw FindError(msg: "Startpath does not match find settings")
            }
        }
        return fileResults
    }

    private func getSortByFilePath() -> (FileResult, FileResult) -> Bool {
        { (fr1: FileResult, fr2: FileResult) -> Bool in
            let (fp1, fp2) = self.settings.sortCaseInsensitive
                             ? (fr1.filePath.lowercased(), fr2.filePath.lowercased())
                             : (fr1.filePath, fr2.filePath)
            let (p1, f1) = FileUtil.splitPath(fp1)
            let (p2, f2) = FileUtil.splitPath(fp2)
            if p1 == p2 {
                return f1 < f2
            }
            return p1 < p2
        }
    }

    private func getSortByFileName() -> (FileResult, FileResult) -> Bool {
        { (fr1: FileResult, fr2: FileResult) -> Bool in
            let (fp1, fp2) = self.settings.sortCaseInsensitive
                             ? (fr1.filePath.lowercased(), fr2.filePath.lowercased())
                             : (fr1.filePath, fr2.filePath)
            let (p1, f1) = FileUtil.splitPath(fp1)
            let (p2, f2) = FileUtil.splitPath(fp2)
            if f1 == f2 {
                return p1 < p2
            }
            return f1 < f2
        }
    }

    private func getSortByFileSize() -> (FileResult, FileResult) -> Bool {
        { (fr1: FileResult, fr2: FileResult) -> Bool in
            if fr1.fileSize == fr2.fileSize {
                let sortByFilePath = self.getSortByFilePath()
                return sortByFilePath(fr1, fr2)
            }
            return fr1.fileSize < fr2.fileSize
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
            if fr1.lastMod == fr2.lastMod {
                let sortByFilePath = self.getSortByFilePath()
                return sortByFilePath(fr1, fr2)
            }
            return fr1.lastMod < fr2.lastMod
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

    public func find() throws -> [FileResult] {
        var fileResults = [FileResult]()
        for p in settings.paths {
            let pathResults: [FileResult] = try getFileResults(p)
            fileResults.append(contentsOf: pathResults)
        }
        return sortFileResults(fileResults)
    }
}

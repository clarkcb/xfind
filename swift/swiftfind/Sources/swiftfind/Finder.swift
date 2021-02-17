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
    private var results = [FindResult]()
    private var textFileEncoding: String.Encoding?

    public init(settings: FindSettings, error: NSErrorPointer) {
        self.settings = settings
        validateSettings(error)
    }

    private func strToEncoding(_ encName: String) -> String.Encoding? {
        var encoding: String.Encoding?
        let enc = String.Encoding(rawValue: CFStringConvertEncodingToNSStringEncoding(CFStringConvertIANACharSetNameToEncoding(encName as CFString)))
        if enc.rawValue == 0xFFFF_FFFF {
            encoding = nil
        } else {
            encoding = enc
        }
        return encoding
    }

    private func validateSettings(_ error: NSErrorPointer) {
        if settings.startPath == nil || settings.startPath!.isEmpty {
            setError(error, msg: "Startpath not defined")
        } else if !FileUtil.exists(settings.startPath!) {
            setError(error, msg: "Startpath not found")
        } else if !FileUtil.isReadableFile(settings.startPath!) {
            setError(error, msg: "Startpath not readable")
        } else if settings.findPatterns.isEmpty {
            setError(error, msg: "No find patterns defined")
        } else if settings.linesAfter < 0 {
            setError(error, msg: "Invalid linesafter")
        } else if settings.linesBefore < 0 {
            setError(error, msg: "Invalid linesbefore")
        } else if settings.maxLineLength < 0 {
            setError(error, msg: "Invalid maxlinelength")
        } else {
            let textFileEncoding = strToEncoding(settings.textFileEncoding)
            if textFileEncoding == nil {
                setError(error, msg: "Invalid textfileencoding")
            } else {
                self.textFileEncoding = textFileEncoding
            }
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

    public func isFindDir(_ dirPath: String) -> Bool {
        if FileUtil.isHidden(dirPath), settings.excludeHidden {
            return false
        }
        return filterByPatterns(dirPath, inPatterns: settings.inDirPatterns,
                                outPatterns: settings.outDirPatterns)
    }

    public func isFindFile(_ filePath: String) -> Bool {
        isFindFile(filePath, fileType: fileTypes.getFileType(filePath))
    }

    public func isFindFile(_ filePath: String, fileType: FileType) -> Bool {
        if FileUtil.isHiddenFile(URL(fileURLWithPath: filePath).lastPathComponent), settings.excludeHidden {
            return false
        }
        return (filterByExtensions(FileUtil.getExtension(filePath),
                                   inExtensions: settings.inExtensions,
                                   outExtensions: settings.outExtensions)
                && filterByPatterns(filePath,
                                    inPatterns: settings.inFilePatterns,
                                    outPatterns: settings.outFilePatterns)
                && filterByFileTypes(fileType,
                                     inFileTypes: settings.inFileTypes,
                                     outFileTypes: settings.outFileTypes))
    }

    public func isArchiveFindFile(_ filePath: String) -> Bool {
        if FileUtil.isHidden(URL(fileURLWithPath: filePath).lastPathComponent), settings.excludeHidden {
            return false
        }
        return (filterByExtensions(FileUtil.getExtension(filePath),
                                   inExtensions: settings.inArchiveExtensions,
                                   outExtensions: settings.outArchiveExtensions)
                && filterByPatterns(filePath, inPatterns: settings.inArchiveFilePatterns,
                                    outPatterns: settings.outArchiveFilePatterns))
    }

    public func find(_ error: NSErrorPointer) {
        let startPath = settings.startPath!
        if FileUtil.isDirectory(startPath) {
            if isFindDir(startPath) {
                findPath(startPath)
            } else {
                setError(error, msg: "Startpath does not match find settings")
            }
        } else if FileUtil.isReadableFile(startPath) {
            let fileType = fileTypes.getFileType(startPath)
            if isFindFile(startPath, fileType: fileType) {
                findFile(FindFile(filePath: startPath, fileType: fileType))
            } else {
                setError(error, msg: "Startpath does not match find settings")
            }
        } else {
            setError(error, msg: "Startpath not readable")
        }
    }

    private func findPath(_ filePath: String) {
        var findFiles: [FindFile] = getFindFiles(filePath)
        findFiles = findFiles.sorted(by: { $0.filePath < $1.filePath })

        if settings.verbose {
            let findDirs = findFiles.map {
                URL(fileURLWithPath: $0.filePath).deletingLastPathComponent().path
            }.sorted().unique()
            logMsg("\nDirectories to be found (\(findDirs.count)):")
            for dir in findDirs {
                logMsg(FileUtil.formatPath(dir, forPath: settings.startPath!))
            }

            logMsg("\nFiles to be found (\(findFiles.count)):")
            for file in findFiles {
                logMsg(FileUtil.formatPath(file.filePath, forPath: settings.startPath!))
            }
            logMsg("")
        }

        for file in findFiles {
            findFile(file)
        }
    }

    // gets all FindFiles recursively
    private func getFindFiles(_ filePath: String) -> [FindFile] {
        var findFiles = [FindFile]()
        if let enumerator = FileUtil.enumerator(forPath: filePath, settings: settings) {
            for case let fileURL as URL in enumerator {
                do {
                    let fileAttributes = try fileURL.resourceValues(forKeys: [.isDirectoryKey, .isRegularFileKey])
                    if fileAttributes.isDirectory! {
                        if !isFindDir(fileURL.path) {
                            enumerator.skipDescendents()
                        }
                    } else if fileAttributes.isRegularFile! {
                        if let findFile = filterToFindFile(fileURL.path) {
                            findFiles.append(findFile)
                        }
                    }
                } catch { print(error, fileURL) }
            }
        }
        return findFiles
    }

    public func filterToFindFile(_ filePath: String) -> FindFile? {
        let fileType = fileTypes.getFileType(filePath)
        if fileType == FileType.unknown {
            return nil
        }
        if (fileType == FileType.archive && settings.findArchives && isArchiveFindFile(filePath))
            || (!settings.archivesOnly && isFindFile(filePath, fileType: fileType))
        {
            return FindFile(filePath: filePath, fileType: fileType)
        }
        return nil
    }

    public func filterFile(_ filePath: String) -> Bool {
        let fileType = fileTypes.getFileType(filePath)
        if fileType == FileType.unknown {
            return false
        }
        if fileType == FileType.archive {
            return settings.findArchives && isArchiveFindFile(filePath)
        }
        // fileType == FileType.Text || fileType == FileType.Binary
        return !settings.archivesOnly && isFindFile(filePath, fileType: fileType)
    }

    func findFile(_ findFile: FindFile) {
        if findFile.fileType == FileType.code || findFile.fileType == FileType.text
            || findFile.fileType == FileType.xml
        {
            findTextFile(findFile)
        } else if findFile.fileType == FileType.binary {
            findBinaryFile(findFile)
        } else if findFile.fileType == FileType.archive {
            findArchiveFile(findFile)
        }
    }

    private func findTextFile(_ findFile: FindFile) {
        if settings.multiLineFind {
            findTextFileContents(findFile)
        } else {
            findTextFileLines(findFile)
        }
    }

    private func findTextFileContents(_ findFile: FindFile) {
        let contents = try? String(contentsOfFile: findFile.filePath,
                                   encoding: textFileEncoding!)
        if contents != nil {
            let results = findMultiLineString(contents!)
            // add filePath
            for res in results {
                let result = FindResult(
                    findPattern: res.findPattern,
                    file: findFile,
                    lineNum: res.lineNum,
                    matchStartIndex: res.matchStartIndex,
                    matchEndIndex: res.matchEndIndex,
                    line: res.line,
                    linesBefore: res.linesBefore,
                    linesAfter: res.linesAfter
                )
                addFindResult(result)
            }
        }
    }

    public func findMultiLineString(_ str: String) -> [FindResult] {
        var sResults = [FindResult]()
        for pat in settings.findPatterns {
            sResults += findMultiLineStringForPattern(str, pattern: pat)
        }
        return sResults
    }

    private func findMultiLineStringForPattern(_ str: String, pattern: Regex) -> [FindResult] {
        var spResults = [FindResult]()
        let newLineIndices = getNewLineIndices(str)
        let startLineIndices = [0] + newLineIndices.map { $0 + 1 }
        let endLineIndices = newLineIndices + [str.lengthOfBytes(using: String.Encoding.utf8) - 1]
        var matches: [NSTextCheckingResult] = []
        if settings.firstMatch {
            if let match = pattern.firstMatch(str) {
                matches = [match]
            }
        } else {
            matches = pattern.matches(str)
        }
        for match in matches {
            let beforeStartLineIndices = startLineIndices.filter { $0 < match.range.location }
            let startLineIndex = beforeStartLineIndices[beforeStartLineIndices.count - 1]
            let endLineIndex = endLineIndices[beforeStartLineIndices.count - 1]
            let line = lineFromIndices(str, startLineIndex: startLineIndex, endLineIndex: endLineIndex)
            var linesBefore: [String] = []
            if settings.linesBefore > 0 {
                let linesBeforeStartIndices = takeRight(beforeStartLineIndices
                    .filter { $0 < startLineIndex }, num: settings.linesBefore)
                let linesBeforeEndIndices = takeRight(endLineIndices.filter { $0 < endLineIndex },
                                                      num: settings.linesBefore)
                for i in 0 ..< linesBeforeStartIndices.count {
                    linesBefore.append(lineFromIndices(str, startLineIndex: linesBeforeStartIndices[i],
                                                       endLineIndex: linesBeforeEndIndices[i]))
                }
            }
            var linesAfter: [String] = []
            if settings.linesAfter > 0 {
                let linesAfterStartIndices = take(startLineIndices.filter { $0 > startLineIndex },
                                                  num: settings.linesAfter)
                let linesAfterEndIndices = take(endLineIndices.filter { $0 > endLineIndex },
                                                num: settings.linesAfter)
                for i in 0 ..< linesAfterStartIndices.count {
                    linesAfter.append(lineFromIndices(str, startLineIndex: linesAfterStartIndices[i],
                                                      endLineIndex: linesAfterEndIndices[i]))
                }
            }

            if linesBefore.isEmpty || linesBeforeMatch(linesBefore),
               linesAfter.isEmpty || linesAfterMatch(linesAfter)
            {
                let result = FindResult(
                    findPattern: pattern.pattern,
                    file: nil,
                    lineNum: beforeStartLineIndices.count,
                    matchStartIndex: match.range.location - startLineIndex + 1,
                    matchEndIndex: match.range.location + match.range.length - startLineIndex + 1,
                    line: line,
                    linesBefore: linesBefore,
                    linesAfter: linesAfter
                )
                spResults.append(result)
            }
        }
        return spResults
    }

    private func linesMatch(_ lines: [String], _ inPatterns: [Regex], _ outPatterns: [Regex]) -> Bool {
        (inPatterns.isEmpty || anyMatchesAnyPattern(lines, inPatterns))
            && (outPatterns.isEmpty || !anyMatchesAnyPattern(lines, outPatterns))
    }

    private func linesBeforeMatch(_ linesBefore: [String]) -> Bool {
        linesBefore.isEmpty || linesMatch(linesBefore, settings.inLinesBeforePatterns,
                                          settings.outLinesBeforePatterns)
    }

    private func linesAfterMatch(_ linesAfter: [String]) -> Bool {
        linesAfter.isEmpty || linesMatch(linesAfter, settings.inLinesAfterPatterns,
                                         settings.outLinesAfterPatterns)
    }

    private func lineFromIndices(_ str: String, startLineIndex: Int, endLineIndex: Int) -> String {
        let startLineStringIndex = str.index(str.startIndex, offsetBy: startLineIndex)
        let endLineStringIndex = str.index(str.startIndex, offsetBy: endLineIndex)
        return String(str[startLineStringIndex ..< endLineStringIndex])
    }

    private func getNewLineIndices(_ str: String) -> [Int] {
        var indices = [Int]()
        var currentIndex = 0
        for c in str {
            if c == "\n" {
                indices.append(currentIndex)
            }
            currentIndex += 1
        }
        return indices
    }

    private func findTextFileLines(_ findFile: FindFile) {
        let results: [FindResult]
        if let reader = StreamReader(path: findFile.filePath, encoding: textFileEncoding!) {
            results = findLineReader(reader)
            for res in results {
                let result = FindResult(
                    findPattern: res.findPattern,
                    file: findFile,
                    lineNum: res.lineNum,
                    matchStartIndex: res.matchStartIndex,
                    matchEndIndex: res.matchEndIndex,
                    line: res.line,
                    linesBefore: res.linesBefore,
                    linesAfter: res.linesAfter
                )
                addFindResult(result)
            }
            reader.close()
        }
    }

    open func findLineReader(_ reader: StreamReader) -> [FindResult] {
        var stop = false
        var lineNum = 0
        var matchedPatterns: Set<String> = []
        var results: [FindResult] = []
        var linesBefore: [String] = []
        var linesAfter: [String] = []
        while !stop {
            lineNum += 1
            var line: String?
            if linesAfter.count > 0 {
                line = linesAfter.remove(at: 0) as String?
            } else {
                line = reader.nextLine()
            }
            if line == nil {
                stop = true
            } else if settings.firstMatch, settings.findPatterns.count == matchedPatterns.count {
                stop = true
            } else {
                if settings.linesAfter > 0 {
                    while linesAfter.count < settings.linesAfter {
                        if let lineAfter = reader.nextLine() {
                            linesAfter.append(lineAfter)
                        } else {
                            break
                        }
                    }
                }
                let findPatterns = settings.findPatterns.filter { !matchedPatterns.contains($0.pattern) }
                for pat in findPatterns {
                    let matches = pat.matches(line!)
                    for match in matches {
                        if linesBefore.isEmpty || linesBeforeMatch(linesBefore),
                           linesAfter.isEmpty || linesAfterMatch(linesAfter)
                        {
                            let result = FindResult(
                                findPattern: pat.pattern,
                                file: nil,
                                lineNum: lineNum,
                                matchStartIndex: match.range.location + 1,
                                matchEndIndex: match.range.location + match.range.length + 1,
                                line: line!,
                                linesBefore: linesBefore,
                                linesAfter: linesAfter
                            )
                            results.append(result)
                            if settings.firstMatch {
                                matchedPatterns.insert(pat.pattern)
                            }
                        }
                    }
                }
                if settings.linesBefore > 0 {
                    if linesBefore.count == settings.linesBefore {
                        linesBefore.remove(at: 0)
                    }
                    if linesBefore.count < settings.linesBefore {
                        linesBefore.append(line!)
                    }
                }
            }
        }
        return results
    }

    private func findBinaryFile(_ findFile: FindFile) {
        if let data = try? Data(contentsOf: URL(fileURLWithPath: findFile.filePath)) {
            // convert to a string using (any) single-byte encoding, using UTF8
            // should cause conversion problems
            if let dstr = NSString(data: data, encoding: String.Encoding.isoLatin1.rawValue) {
                for pat in settings.findPatterns {
                    var matches = pat.matches(dstr as String)
                    if matches.count > 0, settings.firstMatch {
                        matches = [matches[0]]
                    }
                    for match in matches {
                        let result = FindResult(
                            findPattern: pat.pattern,
                            file: findFile,
                            lineNum: 0,
                            matchStartIndex: match.range.location + 1,
                            matchEndIndex: match.range.location + match.range.length + 1,
                            line: "",
                            linesBefore: [],
                            linesAfter: []
                        )
                        results.append(result)
                    }
                }
            } else {
                logMsg("Problem encountered creating binary string \(findFile.description)")
            }
        } else {
            logMsg("Problem encountered reading binary file \(findFile.description)")
        }
    }

    private func findArchiveFile(_ findFile: FindFile) {
        logMsg("findArchiveFile(filePath=\"\(findFile.description)\")")
    }

    private func addFindResult(_ result: FindResult) {
        results.append(result)
    }

    private func cmpResultsInDir(_ res1: FindResult, _ res2: FindResult) -> Bool {
        let path1 = NSURL(fileURLWithPath: res1.file!.filePath).deletingLastPathComponent?.absoluteString
        let path2 = NSURL(fileURLWithPath: res2.file!.filePath).deletingLastPathComponent?.absoluteString
        if path1 == path2 {
            let file1 = NSURL(fileURLWithPath: res1.file!.filePath).lastPathComponent!.lowercased()
            let file2 = NSURL(fileURLWithPath: res2.file!.filePath).lastPathComponent!.lowercased()
            if file1 == file2 {
                if res1.lineNum == res2.lineNum {
                    return res1.matchStartIndex < res2.matchStartIndex
                }
                return res1.lineNum < res2.lineNum
            }
            return file1 < file2
        }
        return path1 < path2
    }

    // if results weren't in DESC order, would use this method to sort them
    private func getSortedFindResults() -> [FindResult] {
        results.sorted(by: { self.cmpResultsInDir($0, $1) })
    }

    public func getFindResults() -> [FindResult] {
//        return results.reversed() // reverse to get ASC order
        results // reverse to get ASC order
    }
}

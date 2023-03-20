//
//  FindSettings.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public enum SortBy {
    case filePath, fileName, fileSize, fileType, lastMod
}

func nameToSortBy(_ sortByName: String) -> SortBy {
    let lname = sortByName.lowercased()
    switch lname {
    case "name":
        return SortBy.fileName
    case "size":
        return SortBy.fileSize
    case "type":
        return SortBy.fileType
    case "lastmod":
        return SortBy.lastMod
    default:
        return SortBy.filePath
    }
}

func sortByToName(_ sortBy: SortBy) -> String {
    switch sortBy {
    case SortBy.fileName:
        return "name"
    case SortBy.fileSize:
        return "size"
    case SortBy.fileType:
        return "type"
    case SortBy.lastMod:
        return "lastmod"
    default:
        return "path"
    }
}

public enum DefaultSettings {
    public static let archivesOnly = false
    public static let debug = false
    public static let excludeHidden = true
    public static let includeArchives = false
    public static let listDirs = false
    public static let listFiles = false
    public static let maxLastMod: Date? = nil
    public static let maxSize: UInt64 = 0
    public static let minLastMod: Date? = nil
    public static let minSize: UInt64 = 0
    public static let printUsage = false
    public static let printVersion = false
    public static let recursive = true
    public static let sortCaseInsensitive = false
    public static let sortDescending = false
    public static let verbose = false
}

public class FindSettings: CustomStringConvertible {
    private var _archivesOnly: Bool = DefaultSettings.archivesOnly
    private var _debug: Bool = DefaultSettings.debug
    public var excludeHidden: Bool = DefaultSettings.excludeHidden
    public var includeArchives: Bool = DefaultSettings.includeArchives
    public var listDirs: Bool = DefaultSettings.listDirs
    public var listFiles: Bool = DefaultSettings.listFiles
    public var maxLastMod: Date? = DefaultSettings.maxLastMod
    public var maxSize: UInt64 = DefaultSettings.maxSize
    public var minLastMod: Date? = DefaultSettings.minLastMod
    public var minSize: UInt64 = DefaultSettings.minSize
    public var printUsage: Bool = DefaultSettings.printUsage
    public var printVersion: Bool = DefaultSettings.printVersion
    public var recursive: Bool = DefaultSettings.recursive
    public var sortCaseInsensitive: Bool = DefaultSettings.sortCaseInsensitive
    public var sortDescending: Bool = DefaultSettings.sortDescending
    public var verbose: Bool = DefaultSettings.verbose

    public var inArchiveExtensions = Set<String>()
    public var inArchiveFilePatterns = [Regex]()
    public var inDirPatterns = [Regex]()
    public var inExtensions = Set<String>()
    public var inFilePatterns = [Regex]()
    public var inFileTypes = [FileType]()
    public var outArchiveExtensions = Set<String>()
    public var outArchiveFilePatterns = [Regex]()
    public var outDirPatterns = [Regex]()
    public var outExtensions = Set<String>()
    public var outFilePatterns = [Regex]()
    public var outFileTypes = [FileType]()
    public var paths = Set<String>()
    public var sortBy = SortBy.filePath

    public init() {}

    fileprivate func splitExtensions(_ exts: String) -> [String] {
        exts.split { $0 == "," }.map { String($0) }
    }

    public func addInArchiveExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            inArchiveExtensions.insert(ext)
        }
    }

    public func addInExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            inExtensions.insert(ext)
        }
    }

    public func addInArchiveFilePattern(_ pattern: String) {
        inArchiveFilePatterns.append(Regex(pattern))
    }

    public func addInDirPattern(_ pattern: String) {
        inDirPatterns.append(Regex(pattern))
    }

    public func addInFilePattern(_ pattern: String) {
        inFilePatterns.append(Regex(pattern))
    }

    public func addInFileType(_ typeName: String) {
        let fileType = FileTypes.fromName(typeName)
        inFileTypes.append(fileType)
        // if text, add text sub-types
        if fileType == FileType.text {
            inFileTypes.append(FileType.code)
            inFileTypes.append(FileType.xml)
        }
    }

    public func addOutArchiveExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            outArchiveExtensions.insert(ext)
        }
    }

    public func addOutExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            outExtensions.insert(ext)
        }
    }

    public func addOutArchiveFilePattern(_ pattern: String) {
        outArchiveFilePatterns.append(Regex(pattern))
    }

    public func addOutDirPattern(_ pattern: String) {
        outDirPatterns.append(Regex(pattern))
    }

    public func addOutFilePattern(_ pattern: String) {
        outFilePatterns.append(Regex(pattern))
    }

    public func addOutFileType(_ typeName: String) {
        let fileType = FileTypes.fromName(typeName)
        outFileTypes.append(fileType)
        // if text, add text sub-types
        if fileType == FileType.text {
            outFileTypes.append(FileType.code)
            outFileTypes.append(FileType.xml)
        }
    }

    public func setMaxLastModFromString(_ maxLastModStr: String) {
        maxLastMod = stringToDate(maxLastModStr)
    }

    public func setMaxSizeFromString(_ maxSizeStr: String) {
        maxSize = UInt64(maxSizeStr) ?? 0
    }

    public func setMinLastModFromString(_ minLastModStr: String) {
        minLastMod = stringToDate(minLastModStr)
    }

    public func setMinSizeFromString(_ minSizeStr: String) {
        minSize = UInt64(minSizeStr) ?? 0
    }

    public func needStat() -> Bool {
        return self.sortBy == SortBy.fileSize || self.sortBy == SortBy.lastMod ||
        self.maxLastMod != nil || self.minLastMod != nil ||
        self.maxSize > 0 || self.minSize > 0
    }

    public func addPath(_ path: String) {
        paths.insert(path)
    }

    public func setSortBy(_ sortByName: String) {
        sortBy = nameToSortBy(sortByName)
    }

    public var archivesOnly: Bool {
        get {
            _archivesOnly
        }
        set {
            _archivesOnly = newValue
            if newValue {
                includeArchives = newValue
            }
        }
    }

    public var debug: Bool {
        get {
            _debug
        }
        set {
            _debug = newValue
            if newValue {
                verbose = newValue
            }
        }
    }

    public var description: String {
        "FindSettings(" +
            "archivesOnly=\(archivesOnly)" +
            ", debug=\(debug)" +
            ", excludeHidden=\(excludeHidden)" +
            ", inArchiveExtensions=\(setToString(inArchiveExtensions))" +
            ", inArchiveFilePatterns=\(arrayToString(inArchiveFilePatterns))" +
            ", inDirPatterns=\(arrayToString(inDirPatterns))" +
            ", inExtensions=\(setToString(inExtensions))" +
            ", inFilePatterns=\(arrayToString(inFilePatterns))" +
            ", inFileTypes=\(arrayToString(inFileTypes.map { FileTypes.toName($0) }))" +
            ", includeArchives=\(includeArchives)" +
            ", listDirs=\(listDirs)" +
            ", listFiles=\(listFiles)" +
            ", maxLastMod=\(dateToString(maxLastMod))" +
            ", maxSize=\(maxSize)" +
            ", minLastMod=\(dateToString(minLastMod))" +
            ", minSize=\(minSize)" +
            ", outArchiveExtensions=\(setToString(outArchiveExtensions))" +
            ", outArchiveFilePatterns=\(arrayToString(outArchiveFilePatterns))" +
            ", outDirPatterns=\(arrayToString(outDirPatterns))" +
            ", outExtensions=\(setToString(outExtensions))" +
            ", outFilePatterns=\(arrayToString(outFilePatterns))" +
            ", outFileTypes=\(arrayToString(outFileTypes.map { FileTypes.toName($0) }))" +
            ", paths=\(setToString(paths))" +
            ", printUsage=\(printUsage)" +
            ", printVersion=\(printVersion)" +
            ", recursive=\(recursive)" +
            ", sortBy=\(sortByToName(sortBy))" +
            ", sortCaseInsensitive=\(sortCaseInsensitive)" +
            ", sortDescending=\(sortDescending)" +
            ", verbose=\(verbose)" +
            ")"
    }
}

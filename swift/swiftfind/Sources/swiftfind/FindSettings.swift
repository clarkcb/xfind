//
//  FindSettings.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

fileprivate let sortByFilePathName = "filepath"
fileprivate let sortByFileNameName = "filename"
fileprivate let sortByNameName = "name"
fileprivate let sortByFileSizeName = "filesize"
fileprivate let sortBySizeName = "size"
fileprivate let sortByFileTypeName = "filetype"
fileprivate let sortByTypeName = "type"
fileprivate let sortByLastModName = "lastmod"

public enum SortBy {
    case filePath, fileName, fileSize, fileType, lastMod
}

public func nameToSortBy(_ sortByName: String) -> SortBy {
    let lname = sortByName.lowercased()
    switch lname {
    case sortByFileNameName, sortByNameName:
        return SortBy.fileName
    case sortByFileSizeName, sortBySizeName:
        return SortBy.fileSize
    case sortByFileTypeName, sortByTypeName:
        return SortBy.fileType
    case sortByLastModName:
        return SortBy.lastMod
    default:
        return SortBy.filePath
    }
}

public func sortByToName(_ sortBy: SortBy) -> String {
    switch sortBy {
    case SortBy.fileName:
        sortByFileNameName
    case SortBy.fileSize:
        sortByFileSizeName
    case SortBy.fileType:
        sortByFileTypeName
    case SortBy.lastMod:
        sortByLastModName
    default:
        sortByFilePathName
    }
}

public enum DefaultFindSettings {
    public static let archivesOnly = false
    public static let debug = false
    public static let includeArchives = false
    public static let includeHidden = false
    public static let listDirs = false
    public static let listFiles = false
    public static let maxDepth: Int64 = -1
    public static let maxLastMod: Date? = nil
    public static let maxSize: UInt64 = 0
    public static let minDepth: Int64 = -1
    public static let minLastMod: Date? = nil
    public static let minSize: UInt64 = 0
    public static let printUsage = false
    public static let printVersion = false
    public static let recursive = true
    public static let sortCaseInsensitive = false
    public static let sortDescending = false
    public static let verbose = false
}

open class FindSettings: CustomStringConvertible {
    // these are public because they must be accessible outside the package
    public var _archivesOnly: Bool = DefaultFindSettings.archivesOnly
    public var _debug: Bool = DefaultFindSettings.debug
    open var includeArchives: Bool = DefaultFindSettings.includeArchives
    open var includeHidden: Bool = DefaultFindSettings.includeHidden
    open var listDirs: Bool = DefaultFindSettings.listDirs
    open var listFiles: Bool = DefaultFindSettings.listFiles
    open var maxDepth: Int64 = DefaultFindSettings.maxDepth
    open var maxLastMod: Date? = DefaultFindSettings.maxLastMod
    open var maxSize: UInt64 = DefaultFindSettings.maxSize
    open var minDepth: Int64 = DefaultFindSettings.minDepth
    open var minLastMod: Date? = DefaultFindSettings.minLastMod
    open var minSize: UInt64 = DefaultFindSettings.minSize
    open var printUsage: Bool = DefaultFindSettings.printUsage
    open var printVersion: Bool = DefaultFindSettings.printVersion
    open var recursive: Bool = DefaultFindSettings.recursive
    open var sortCaseInsensitive: Bool = DefaultFindSettings.sortCaseInsensitive
    open var sortDescending: Bool = DefaultFindSettings.sortDescending
    open var verbose: Bool = DefaultFindSettings.verbose

    open var inArchiveExtensions = Set<String>()
    open var inArchiveFilePatterns = [Regex]()
    open var inDirPatterns = [Regex]()
    open var inExtensions = Set<String>()
    open var inFilePatterns = [Regex]()
    open var inFileTypes = [FileType]()
    open var outArchiveExtensions = Set<String>()
    open var outArchiveFilePatterns = [Regex]()
    open var outDirPatterns = [Regex]()
    open var outExtensions = Set<String>()
    open var outFilePatterns = [Regex]()
    open var outFileTypes = [FileType]()
    open var paths = Set<String>()
    open var sortBy = SortBy.filePath

    public init() {}

    func splitExtensions(_ exts: String) -> [String] {
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

    public func setMaxDepthFromString(_ maxDepthStr: String) {
        maxDepth = Int64(maxDepthStr) ?? 0
    }

    public func setMaxLastModFromString(_ maxLastModStr: String) {
        maxLastMod = stringToDate(maxLastModStr)
    }

    public func setMaxSizeFromString(_ maxSizeStr: String) {
        maxSize = UInt64(maxSizeStr) ?? 0
    }

    public func setMinDepthFromString(_ minDepthStr: String) {
        minDepth = Int64(minDepthStr) ?? 0
    }

    public func setMinLastModFromString(_ minLastModStr: String) {
        minLastMod = stringToDate(minLastModStr)
    }

    public func setMinSizeFromString(_ minSizeStr: String) {
        minSize = UInt64(minSizeStr) ?? 0
    }

    public func needStat() -> Bool {
        sortBy == SortBy.fileSize || sortBy == SortBy.lastMod ||
            maxLastMod != nil || minLastMod != nil ||
            maxSize > 0 || minSize > 0
    }

    public func addPath(_ path: String) {
        paths.insert(path)
    }

    public func setSortBy(_ sortByName: String) {
        sortBy = nameToSortBy(sortByName)
    }

    open var archivesOnly: Bool {
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

    open var debug: Bool {
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

    open var description: String {
        "FindSettings(" +
            "archivesOnly=\(archivesOnly)" +
            ", debug=\(debug)" +
            ", inArchiveExtensions=\(setToString(inArchiveExtensions))" +
            ", inArchiveFilePatterns=\(arrayToString(inArchiveFilePatterns))" +
            ", inDirPatterns=\(arrayToString(inDirPatterns))" +
            ", inExtensions=\(setToString(inExtensions))" +
            ", inFilePatterns=\(arrayToString(inFilePatterns))" +
            ", inFileTypes=\(arrayToString(inFileTypes.map { FileTypes.toName($0) }, false))" +
            ", includeArchives=\(includeArchives)" +
            ", includeHidden=\(includeHidden)" +
            ", listDirs=\(listDirs)" +
            ", listFiles=\(listFiles)" +
            ", maxDepth=\(maxDepth)" +
            ", maxLastMod=\(dateToString(maxLastMod))" +
            ", maxSize=\(maxSize)" +
            ", minDepth=\(minDepth)" +
            ", minLastMod=\(dateToString(minLastMod))" +
            ", minSize=\(minSize)" +
            ", outArchiveExtensions=\(setToString(outArchiveExtensions))" +
            ", outArchiveFilePatterns=\(arrayToString(outArchiveFilePatterns))" +
            ", outDirPatterns=\(arrayToString(outDirPatterns))" +
            ", outExtensions=\(setToString(outExtensions))" +
            ", outFilePatterns=\(arrayToString(outFilePatterns))" +
            ", outFileTypes=\(arrayToString(outFileTypes.map { FileTypes.toName($0) }, false))" +
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

//
//  FileUtil.swift
//  swiftfind
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public enum FileUtil {
    fileprivate static let dotDirs = Set<String>([".", "..", "./", "../"])
    fileprivate static let separator = "/"

    public static func contractPath(_ filePath: String) -> String {
        // NOTE: I get this error for the following line when trying to build on linux:
        //       value of type 'NSString' has no member 'abbreviatingWithTildeInPath'
        // (filePath as NSString).abbreviatingWithTildeInPath
        if filePath.hasPrefix(NSHomeDirectory()) {
            return filePath.replacingOccurrences(of: NSHomeDirectory(), with: "~")
        }
        return filePath
    }

    public static func relativePath(_ filePath: String, forPath: String) -> String {
        if forPath == "." || forPath.hasPrefix("./") {
            let fullForPath = URL(fileURLWithPath: ".").path
            return filePath.replacingOccurrences(of: fullForPath, with: ".")
        }
        if forPath == ".." || forPath.hasPrefix("../") {
            let fullForPath = URL(fileURLWithPath: "..").path
            return filePath.replacingOccurrences(of: fullForPath, with: "..")
        }
        return filePath
    }

    // this formats filePath according to forPath, which means that it will become relative
    // if forPath is, or the the HOME prefix will be replaced with tilde if forPath.hasPrefix("~")
    public static func formatPath(_ filePath: String, forPath: String) -> String {
        if forPath.hasPrefix("~") {
            return contractPath(filePath)
        }
        return relativePath(filePath, forPath: forPath)
    }

    public static func formatPath(_ filePath: String, forPaths: [String]) -> String {
        for p in forPaths {
            let formatted: String = formatPath(filePath, forPath: p)
            if formatted.count < filePath.count {
                return formatted
            }
        }
        return filePath
    }

    public static func expandPath(_ filePath: String) -> String {
        (filePath as NSString).expandingTildeInPath
    }

    public static func getExtension(_ fileName: String) -> String {
        let ext = NSURL(fileURLWithPath: fileName).pathExtension?.uppercased()
        if ext == "Z" {
            return ext!
        }
        return ext!.lowercased()
    }

    public static func hasExtension(_ fileName: String, ext: String) -> Bool {
        getExtension(fileName) == ext.lowercased()
    }

    fileprivate static func getFileManager() -> FileManager {
        FileManager.default
    }

    fileprivate static func getOptions(forSettings settings: FindSettings) -> FileManager.DirectoryEnumerationOptions {
        var options: FileManager.DirectoryEnumerationOptions = [.skipsPackageDescendants]
        if !settings.includeHidden {
            options.insert(.skipsHiddenFiles)
        }
        if !settings.recursive {
            options.insert(.skipsSubdirectoryDescendants)
        }
        return options
    }

    // gets files only directly under given path --> set settings.recursive to false and call `enumerator` below instead
    public static func directoryContents(forPath filePath: String, settings: FindSettings) -> [String] {
        do {
            let options = getOptions(forSettings: settings)
            let fileUrls = try getFileManager().contentsOfDirectory(
                at: URL(fileURLWithPath: expandPath(filePath)),
                includingPropertiesForKeys: [.isRegularFileKey],
                options: options
            )
            return fileUrls.map(\.path)
        } catch {
            return []
        }
    }

    // gets files recursively under given path
    public static func enumerator(forPath filePath: String,
                                  settings: FindSettings) -> FileManager.DirectoryEnumerator? {
        let options = getOptions(forSettings: settings)
        return getFileManager().enumerator(at: URL(fileURLWithPath: expandPath(filePath)),
                                           includingPropertiesForKeys: [.isDirectoryKey, .isRegularFileKey],
                                           options: options)
    }

    public static func exists(_ filePath: String) -> Bool {
        getFileManager().fileExists(atPath: filePath)
    }

    public static func isDirectory(_ filePath: String) -> Bool {
        var isDir: ObjCBool = false
        if getFileManager().fileExists(atPath: expandPath(filePath), isDirectory: &isDir) {
            return isDir.boolValue
        }
        return false
    }

    public static func getFileAttributes(_ filePath: String) throws -> [FileAttributeKey: Any] {
        return try getFileManager().attributesOfItem(atPath: filePath)
    }

    public static func isReadableFile(_ filePath: String) -> Bool {
        getFileManager().isReadableFile(atPath: expandPath(filePath))
    }

    public static func isSymlink(_ filePath: String) -> Bool {
        let url = URL(fileURLWithPath: filePath)
        if let ok = try? url.checkResourceIsReachable(), ok {
            let vals = try? url.resourceValues(forKeys: [.isSymbolicLinkKey])
            if let islink = vals?.isSymbolicLink, islink {
                return islink
            }
        }
        return false
    }

    public static func getSymlinkTarget(_ filePath: String) -> String? {
        return try? getFileManager().destinationOfSymbolicLink(atPath: expandPath(filePath))
    }

    public static func isDotDir(_ filePath: String) -> Bool {
        dotDirs.contains(filePath)
    }

    public static func isHiddenName(_ name: String) -> Bool {
        name.count > 1 && name.hasPrefix(".") && !isDotDir(name)
    }

    public static func isHiddenPath(_ filePath: String) -> Bool {
        filePath.split { $0 == "/" }.map { String($0) }.contains { isHiddenName($0) }
    }

    public static func splitPath(_ filePath: String) -> (String, String) {
        var fp = filePath
        if fp.hasSuffix(separator) {
            fp = String(fp.dropLast())
        }
        if let idx = fp.lastIndex(of: separator[separator.startIndex]) {
            return (String(fp[..<idx]), String(fp[fp.index(after: idx)...]))
        }
        return ("", filePath)
    }

//    public static func splitPathWithURL(_ filePath: String) -> (directory: String, fileName: String) {
//      let url = URL(fileURLWithPath: filePath)
//      return (url.deletingLastPathComponent().path, url.lastPathComponent)
//    }

    public static func joinPath(_ path: String, childPath: String) -> String {
        if path.hasSuffix(separator) {
            "\(path)\(childPath)"
        } else {
            "\(path)\(separator)\(childPath)"
        }
    }
}

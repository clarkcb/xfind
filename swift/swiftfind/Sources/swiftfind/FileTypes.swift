//
//  FileTypes.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public enum FileType {
    case unknown, archive, binary, code, text, xml
}

public class FileTypes {
    fileprivate static let archive = "archive"
    fileprivate static let binary = "binary"
    fileprivate static let code = "code"
    fileprivate static let text = "text"
    fileprivate static let unknown = "unknown"
    fileprivate static let xml = "xml"

    private var config: Config
    private var fileTypeExtDict = [String: Set<String>]()
    private var fileTypeNameDict = [String: Set<String>]()

    public init() {
        self.config = Config()
        setFileTypesFromJson()
    }

    private func setFileTypesFromJson() {
        do {
            let fileUrl = URL(fileURLWithPath: config.fileTypesPath)
            let data = try Data(contentsOf: fileUrl, options: .mappedIfSafe)
            if let json = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
                if let filetypes = json["filetypes"] as? [[String: Any]] {
                    for ft in filetypes {
                        let typeName = ft["type"] as! String
                        let extensions = ft["extensions"] as! [String]
                        fileTypeExtDict[typeName] = Set(extensions)
                        let names = ft["names"] as! [String]
                        fileTypeNameDict[typeName] = Set(names)
                    }
                    fileTypeExtDict[FileTypes.text] = fileTypeExtDict[FileTypes.text]!.union(fileTypeExtDict[FileTypes.code]!)
                        .union(fileTypeExtDict[FileTypes.xml]!)
                    fileTypeNameDict[FileTypes.text] = fileTypeNameDict[FileTypes.text]!.union(fileTypeNameDict[FileTypes.code]!)
                        .union(fileTypeNameDict[FileTypes.xml]!)
                }
            }
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    public static func fromName(_ typeName: String) -> FileType {
        let lname = typeName.lowercased()
        switch lname {
        case text:
            return FileType.text
        case binary:
            return FileType.binary
        case archive:
            return FileType.archive
        case code:
            return FileType.code
        case xml:
            return FileType.xml
        default:
            return FileType.unknown
        }
    }

    public static func toName(_ fileType: FileType) -> String {
        switch fileType {
        case FileType.text:
            return "text"
        case FileType.binary:
            return "binary"
        case FileType.archive:
            return "archive"
        case FileType.code:
            return "code"
        case FileType.xml:
            return "xml"
        default:
            return "unknown"
        }
    }

    public func getFileType(_ fileName: String) -> FileType {
        if isCodeFile(fileName) {
            return FileType.code
        }
        if isXmlFile(fileName) {
            return FileType.xml
        }
        if isTextFile(fileName) {
            return FileType.text
        }
        if isBinaryFile(fileName) {
            return FileType.binary
        }
        if isArchiveFile(fileName) {
            return FileType.archive
        }
        return FileType.unknown
    }

    private func isFileOfType(_ fileName: String, _ typeName: String) -> Bool {
        return fileTypeNameDict[typeName]!.contains(fileName)
            || fileTypeExtDict[typeName]!.contains(FileUtil.getExtension(fileName))
    }

    public func isArchiveFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.archive)
    }

    public func isBinaryFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.binary)
    }

    public func isCodeFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.code)
    }

    public func isTextFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.text)
    }

    public func isUnknownFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.unknown
    }

    public func isXmlFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.xml)
    }
}

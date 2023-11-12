//
//  FileTypes.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public enum FileType {
    case unknown, archive, audio, binary, code, font, image, text, video, xml
}

public class FileTypes {
    fileprivate static let archive = "archive"
    fileprivate static let audio = "audio"
    fileprivate static let binary = "binary"
    fileprivate static let code = "code"
    fileprivate static let font = "font"
    fileprivate static let image = "image"
    fileprivate static let text = "text"
    fileprivate static let unknown = "unknown"
    fileprivate static let video = "video"
    fileprivate static let xml = "xml"

    private var config: FindConfig
    private var fileTypeExtDict = [String: Set<String>]()
    private var fileTypeNameDict = [String: Set<String>]()

    public init() {
        config = FindConfig()
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
        case archive:
            return FileType.archive
        case audio:
            return FileType.audio
        case binary:
            return FileType.binary
        case code:
            return FileType.code
        case font:
            return FileType.font
        case image:
            return FileType.image
        case text:
            return FileType.text
        case video:
            return FileType.video
        case xml:
            return FileType.xml
        default:
            return FileType.unknown
        }
    }

    public static func toName(_ fileType: FileType) -> String {
        switch fileType {
        case FileType.archive:
            "archive"
        case FileType.audio:
            "audio"
        case FileType.binary:
            "binary"
        case FileType.code:
            "code"
        case FileType.font:
            "font"
        case FileType.image:
            "image"
        case FileType.text:
            "text"
        case FileType.video:
            "video"
        case FileType.xml:
            "xml"
        default:
            "unknown"
        }
    }

    public func getFileType(_ fileName: String) -> FileType {
        // most specific first
        if isCodeFile(fileName) {
            return FileType.code
        }
        if isArchiveFile(fileName) {
            return FileType.archive
        }
        if isAudioFile(fileName) {
            return FileType.audio
        }
        if isFontFile(fileName) {
            return FileType.font
        }
        if isImageFile(fileName) {
            return FileType.image
        }
        if isVideoFile(fileName) {
            return FileType.video
        }

        // most general last
        if isXmlFile(fileName) {
            return FileType.xml
        }
        if isTextFile(fileName) {
            return FileType.text
        }
        if isBinaryFile(fileName) {
            return FileType.binary
        }
        return FileType.unknown
    }

    private func isFileOfType(_ fileName: String, _ typeName: String) -> Bool {
        fileTypeNameDict[typeName]!.contains(fileName)
            || fileTypeExtDict[typeName]!.contains(FileUtil.getExtension(fileName))
    }

    public func isArchiveFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.archive)
    }

    public func isAudioFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.audio)
    }

    public func isBinaryFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.binary)
    }

    public func isCodeFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.code)
    }

    public func isFontFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.font)
    }

    public func isImageFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.image)
    }

    public func isTextFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.text)
    }

    public func isVideoFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.video)
    }

    public func isUnknownFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.unknown
    }

    public func isXmlFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.xml)
    }
}

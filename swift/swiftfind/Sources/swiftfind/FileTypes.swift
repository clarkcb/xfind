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
    fileprivate static let fileTypeArchiveName = "archive"
    fileprivate static let fileTypeAudioName = "audio"
    fileprivate static let fileTypeBinaryName = "binary"
    fileprivate static let fileTypeCodeName = "code"
    fileprivate static let fileTypeFontName = "font"
    fileprivate static let fileTypeImageName = "image"
    fileprivate static let fileTypeTextName = "text"
    fileprivate static let fileTypeUnknownName = "unknown"
    fileprivate static let fileTypeVideoName = "video"
    fileprivate static let fileTypeXmlName = "xml"

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
                    fileTypeExtDict[FileTypes.fileTypeTextName] = fileTypeExtDict[FileTypes.fileTypeTextName]!
                        .union(fileTypeExtDict[FileTypes.fileTypeCodeName]!)
                        .union(fileTypeExtDict[FileTypes.fileTypeXmlName]!)
                    fileTypeNameDict[FileTypes.fileTypeTextName] = fileTypeNameDict[FileTypes.fileTypeTextName]!
                        .union(fileTypeNameDict[FileTypes.fileTypeCodeName]!)
                        .union(fileTypeNameDict[FileTypes.fileTypeXmlName]!)
                }
            }
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    public static func fromName(_ typeName: String) -> FileType {
        let lname = typeName.lowercased()
        switch lname {
        case fileTypeArchiveName:
            return FileType.archive
        case fileTypeAudioName:
            return FileType.audio
        case fileTypeBinaryName:
            return FileType.binary
        case fileTypeCodeName:
            return FileType.code
        case fileTypeFontName:
            return FileType.font
        case fileTypeImageName:
            return FileType.image
        case fileTypeTextName:
            return FileType.text
        case fileTypeVideoName:
            return FileType.video
        case fileTypeXmlName:
            return FileType.xml
        default:
            return FileType.unknown
        }
    }

    public static func toName(_ fileType: FileType) -> String {
        switch fileType {
        case FileType.archive:
            fileTypeArchiveName
        case FileType.audio:
            fileTypeAudioName
        case FileType.binary:
            fileTypeBinaryName
        case FileType.code:
            fileTypeCodeName
        case FileType.font:
            fileTypeFontName
        case FileType.image:
            fileTypeImageName
        case FileType.text:
            fileTypeTextName
        case FileType.video:
            fileTypeVideoName
        case FileType.xml:
            fileTypeXmlName
        default:
            fileTypeUnknownName
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
        isFileOfType(fileName, FileTypes.fileTypeArchiveName)
    }

    public func isAudioFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.fileTypeAudioName)
    }

    public func isBinaryFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.fileTypeBinaryName)
    }

    public func isCodeFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.fileTypeCodeName)
    }

    public func isFontFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.fileTypeFontName)
    }

    public func isImageFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.fileTypeImageName)
    }

    public func isTextFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.fileTypeTextName)
    }

    public func isVideoFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.fileTypeVideoName)
    }

    public func isUnknownFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.unknown
    }

    public func isXmlFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.fileTypeXmlName)
    }
}

//
//  FileTypes.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
import SQLite

public enum FileType : Int {
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
    private var db: Connection?
    private var extTypeCache = [String: FileType]()
    private var nameTypeCache = [String: FileType]()
    private var nameTypeCacheLoaded = false

    public init() {
        config = FindConfig()
        do {
            db = try Connection(config.xfindDb, readonly: true)
        } catch {
            print("Failed to load: \(error.localizedDescription)")
            db = nil
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

    private func getFileTypesForQueryAndParams(_ query: String, _ params: [String]) -> [String: FileType] {
        var fileTypeMap = [String: FileType]()
        do {
            let stmt = try self.db!.prepare(query, params)
            for row in stmt {
                let key = row[0] as! String
                let fileTypeId = Int(row[1] as! Int64) - 1
                fileTypeMap[key] = FileType(rawValue: fileTypeId)
            }
        } catch {
            print("Failed to execute query: \(error.localizedDescription)")
        }
        return fileTypeMap
    }
    
    private func loadNameTypeCache() -> Void {
        let query = "SELECT name, file_type_id FROM file_name"
        nameTypeCache = getFileTypesForQueryAndParams(query, [])
        nameTypeCacheLoaded = true
    }

    private func getFileTypeForQueryAndParams(_ query: String, _ params: [String]) -> FileType {
        do {
            let stmt = try self.db!.prepare(query, params)
            for row in stmt {
                let fileTypeId = Int(row[0] as! Int64) - 1
                return FileType(rawValue: fileTypeId)!
            }
        } catch {
            print("Failed to execute file_name query: \(error.localizedDescription)")
        }
        return FileType.unknown
    }

    private func getFileTypeFromFileName(_ fileName: String) -> FileType {
        if (!nameTypeCacheLoaded) {
            loadNameTypeCache()
        }
        if nameTypeCache.index(forKey: fileName) != nil {
            return nameTypeCache[fileName]!
        }
//        let query = "SELECT file_type_id FROM file_name WHERE name = ?"
//        return getFileTypeForQueryAndParams(query, [fileName])
        return FileType.unknown
    }

    private func getFileTypeFromExtension(_ fileExt: String) -> FileType {
        if (fileExt.isEmpty) {
            return FileType.unknown
        }
        if extTypeCache.index(forKey: fileExt) != nil {
            return extTypeCache[fileExt]!
        }
        let query = "SELECT file_type_id FROM file_extension WHERE extension = ?"
        let fileType = getFileTypeForQueryAndParams(query, [fileExt])
        extTypeCache[fileExt] = fileType
        return fileType
    }

    public func getFileType(_ fileName: String) -> FileType {
        let fileTypeFromFileName = getFileTypeFromFileName(fileName)
        if fileTypeFromFileName != FileType.unknown {
            return fileTypeFromFileName
        }
        let fileTypeFromExtension = getFileTypeFromExtension(FileUtil.getExtension(fileName))
        return fileTypeFromExtension
    }

    public func isArchiveFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.archive
    }

    public func isAudioFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.audio
    }

    public func isBinaryFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.binary
    }

    public func isCodeFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.code
    }

    public func isFontFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.font
    }

    public func isImageFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.image
    }

    public func isTextFile(_ fileName: String) -> Bool {
        let fileType = getFileType(fileName)
        return (fileType == FileType.text || fileType == FileType.code || fileType == FileType.xml)
    }

    public func isVideoFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.video
    }

    public func isUnknownFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.unknown
    }

    public func isXmlFile(_ fileName: String) -> Bool {
        getFileType(fileName) == FileType.xml
    }
}

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

class FileTypesXmlParser: NSObject, XMLParserDelegate {
    var fileTypeDict: [String: Set<String>] = [:]
    let fileTypeNodeName = "filetype"
    let extensionsNodeName = "extensions"
    let nameAttributeName = "name"
    var element = ""
    var fileTypeName = ""
    var extensions = NSMutableString()

    func parseFile(_ filepath: String) -> [String: Set<String>] {
        if FileManager.default.fileExists(atPath: filepath) {
            let data: Data? = try? Data(contentsOf: URL(fileURLWithPath: filepath))
            let inputStream: InputStream? = InputStream(data: data!)

            let parser: XMLParser? = XMLParser(stream: inputStream!)
            if parser != nil {
                parser!.delegate = self
                parser!.parse()
            }
        } else {
            print("ERROR: filepath not found: \(filepath)")
        }

        return fileTypeDict
    }

    func parser(_: XMLParser, didStartElement elementName: String,
                namespaceURI _: String?, qualifiedName _: String?,
                attributes attributeDict: [String: String])
    {
        element = elementName
        if (elementName as NSString).isEqual(to: fileTypeNodeName) {
            if attributeDict.index(forKey: nameAttributeName) != nil {
                fileTypeName = (attributeDict[nameAttributeName]!)
            }
            extensions = NSMutableString()
            extensions = ""
        }
    }

    func parser(_: XMLParser, foundCharacters string: String) {
        if element == extensionsNodeName {
            extensions.append(string)
        }
    }

    func parser(_: XMLParser, didEndElement elementName: String,
                namespaceURI _: String?, qualifiedName _: String?)
    {
        if (elementName as NSString).isEqual(to: fileTypeNodeName) {
            if !extensions.isEqual(nil) {
                let exts = extensions.components(separatedBy: whitespace)
                fileTypeDict[fileTypeName] = Set(exts)
            }
        }
    }
}

public class FileTypes {
    fileprivate static let archive = "archive"
    fileprivate static let binary = "binary"
    fileprivate static let code = "code"
    fileprivate static let findable = "findable"
    fileprivate static let text = "text"
    fileprivate static let unknown = "unknown"
    fileprivate static let xml = "xml"

    private var fileTypesDict = [String: Set<String>]()

    public init() {
        // setFileTypesFromXml()
        setFileTypesFromJson()
    }

    private func setFileTypesFromXml() {
        let parser = FileTypesXmlParser()
        fileTypesDict = parser.parseFile(Config.fileTypesPath)
        fileTypesDict[FileTypes.text] = fileTypesDict[FileTypes.text]!.union(fileTypesDict[FileTypes.code]!)
            .union(fileTypesDict[FileTypes.xml]!)
        fileTypesDict[FileTypes.findable] =
            fileTypesDict[FileTypes.text]!.union(fileTypesDict[FileTypes.binary]!)
                .union(fileTypesDict[FileTypes.archive]!)
    }

    private func setFileTypesFromJson() {
        do {
            let fileUrl = URL(fileURLWithPath: Config.fileTypesPath)
            let data = try Data(contentsOf: fileUrl, options: .mappedIfSafe)
            if let json = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
                if let filetypes = json["filetypes"] as? [[String: Any]] {
                    for ft in filetypes {
                        let typeName = ft["type"] as! String
                        let extensions = ft["extensions"] as! [String]
                        fileTypesDict[typeName] = Set(extensions)
                    }
                    fileTypesDict[FileTypes.text] = fileTypesDict[FileTypes.text]!.union(fileTypesDict[FileTypes.code]!)
                        .union(fileTypesDict[FileTypes.xml]!)
                    fileTypesDict[FileTypes.findable] =
                        fileTypesDict[FileTypes.text]!.union(fileTypesDict[FileTypes.binary]!)
                            .union(fileTypesDict[FileTypes.archive]!)
                }
            }
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    public static func fromName(_ typeName: String) -> FileType {
        let lname = typeName.lowercased()
        if lname == text {
            return FileType.text
        }
        if lname == binary {
            return FileType.binary
        }
        if lname == archive {
            return FileType.archive
        }
        if lname == code {
            return FileType.code
        }
        if lname == xml {
            return FileType.xml
        }
        return FileType.unknown
    }

    public static func toName(_ fileType: FileType) -> String {
        if fileType == FileType.text {
            return "text"
        }
        if fileType == FileType.binary {
            return "binary"
        }
        if fileType == FileType.archive {
            return "archive"
        }
        if fileType == FileType.code {
            return "code"
        }
        if fileType == FileType.xml {
            return "xml"
        }
        return "unknown"
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
        fileTypesDict.index(forKey: typeName) != nil &&
            fileTypesDict[typeName]!.contains(FileUtil.getExtension(fileName))
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

    public func isFindableFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.findable)
    }

    public func isTextFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.text)
    }

    public func isUnknownFile(_ fileName: String) -> Bool {
        (fileTypesDict.index(forKey: FileTypes.unknown) != nil &&
            fileTypesDict[FileTypes.unknown]!.contains(FileUtil.getExtension(fileName)))
            || !isFindableFile(fileName)
    }

    public func isXmlFile(_ fileName: String) -> Bool {
        isFileOfType(fileName, FileTypes.xml)
    }
}

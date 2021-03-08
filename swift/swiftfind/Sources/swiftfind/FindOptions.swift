//
//  FindOptions.swift
//  swiftfind
//
//  Created by Cary Clark on 5/17/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

struct FindOption {
    let short: String
    let long: String
    let desc: String

    var sortArg: String {
        (short.isEmpty ? short.lowercased() + "@" : "") + long
    }
}

class FindOptionsXmlParser: NSObject, XMLParserDelegate {
    var findOptions = [FindOption]()
    let findOptionNodeName = "findoption"
    let longAttributeName = "long"
    let shortAttributeName = "short"
    var element = ""
    var longName = ""
    var shortName = ""
    var desc = NSMutableString()

    func parseFile(_ filepath: String) -> [FindOption] {
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
        return findOptions
    }

    func parser(_: XMLParser, didStartElement elementName: String,
                namespaceURI _: String?, qualifiedName _: String?,
                attributes attributeDict: [String: String])
    {
        element = elementName
        if (elementName as NSString).isEqual(to: findOptionNodeName) {
            if attributeDict.index(forKey: longAttributeName) != nil {
                longName = (attributeDict[longAttributeName]!)
            }
            if attributeDict.index(forKey: shortAttributeName) != nil {
                shortName = (attributeDict[shortAttributeName]!)
            }
            desc = NSMutableString()
            desc = ""
        }
    }

    func parser(_: XMLParser, foundCharacters string: String) {
        if element == findOptionNodeName {
            desc.append(string)
        }
    }

    func parser(_: XMLParser, didEndElement elementName: String,
                namespaceURI _: String?, qualifiedName _: String?)
    {
        if (elementName as NSString).isEqual(to: findOptionNodeName) {
            if !desc.isEqual(nil) {
                let trimmedDesc = desc.trimmingCharacters(in: whitespace as CharacterSet)
                findOptions.append(FindOption(short: shortName,
                                                  long: longName, desc: trimmedDesc))
            }
        }
    }
}

public class FindOptions {
    private var findOptions = [FindOption]()
    private var longArgDict: [String: String] = [:]

    public init() {
        // setFindOptionsFromXml()
        setFindOptionsFromJson()
    }

    private func setFindOptionsFromXml() {
        let parser = FindOptionsXmlParser()
        findOptions = parser.parseFile(Config.findOptionsPath)
        findOptions.sort(by: { $0.sortArg < $1.sortArg })
        for opt in findOptions {
            longArgDict[opt.long] = opt.long
            if !opt.short.isEmpty {
                longArgDict[opt.short] = opt.long
            }
        }
    }

    private func setFindOptionsFromJson() {
        do {
            let findOptionsUrl = URL(fileURLWithPath: Config.findOptionsPath)
            let data = try Data(contentsOf: findOptionsUrl, options: .mappedIfSafe)
            if let json = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
                if let options = json["findoptions"] as? [[String: Any]] {
                    for so in options {
                        let longArg = so["long"] as! String
                        let shortArg = so.index(forKey: "short") != nil ? so["short"] as! String : ""
                        let desc = so["desc"] as! String
                        findOptions.append(FindOption(short: shortArg, long: longArg, desc: desc))
                    }
                    findOptions.sort(by: { $0.sortArg < $1.sortArg })
                    for opt in findOptions {
                        longArgDict[opt.long] = opt.long
                        if !opt.short.isEmpty {
                            longArgDict[opt.short] = opt.long
                        }
                    }
                }
            }
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    // this is computed property so that it can reference self
    private var argActionDict: [String: (String, FindSettings) -> Void] {
        [
            "in-archiveext": { (str: String, settings: FindSettings) -> Void in
                settings.addInArchiveExtension(str)
            },
            "in-archivefilepattern": { (str: String, settings: FindSettings) -> Void in
                settings.addInArchiveFilePattern(str)
            },
            "in-dirpattern": { (str: String, settings: FindSettings) -> Void in
                settings.addInDirPattern(str)
            },
            "in-ext": { (str: String, settings: FindSettings) -> Void in
                settings.addInExtension(str)
            },
            "in-filepattern": { (str: String, settings: FindSettings) -> Void in
                settings.addInFilePattern(str)
            },
            "in-filetype": { (str: String, settings: FindSettings) -> Void in
                settings.addInFileType(str)
            },
            "out-archiveext": { (str: String, settings: FindSettings) -> Void in
                settings.addOutArchiveExtension(str)
            },
            "out-archivefilepattern": { (str: String, settings: FindSettings) -> Void in
                settings.addOutArchiveFilePattern(str)
            },
            "out-dirpattern": { (str: String, settings: FindSettings) -> Void in
                settings.addOutDirPattern(str)
            },
            "out-ext": { (str: String, settings: FindSettings) -> Void in
                settings.addOutExtension(str)
            },
            "out-filepattern": { (str: String, settings: FindSettings) -> Void in
                settings.addOutFilePattern(str)
            },
            "out-filetype": { (str: String, settings: FindSettings) -> Void in
                settings.addOutFileType(str)
            },
            "path": { (str: String, settings: FindSettings) -> Void in
                settings.addPath(str)
            },
            "settings-file": { (str: String, settings: FindSettings) -> Void in
                var error: NSError?
                self.addSettingsFromFile(str, settings: settings, error: &error)
            },
        ]
    }

    private let boolFlagActionDict: [String: (Bool, FindSettings) -> Void] = [
        "archivesonly": { (bool: Bool, settings: FindSettings) -> Void in
            settings.archivesOnly = bool
        },
        "colorize": { (bool: Bool, settings: FindSettings) -> Void in
            settings.colorize = bool
        },
        "debug": { (bool: Bool, settings: FindSettings) -> Void in
            settings.debug = bool
        },
        "excludearchives": { (bool: Bool, settings: FindSettings) -> Void in
            settings.includeArchives = !bool
        },
        "excludehidden": { (bool: Bool, settings: FindSettings) -> Void in
            settings.excludeHidden = bool
        },
        "help": { (bool: Bool, settings: FindSettings) -> Void in
            settings.printUsage = bool
        },
        "includearchives": { (bool: Bool, settings: FindSettings) -> Void in
            settings.includeArchives = bool
        },
        "includehidden": { (bool: Bool, settings: FindSettings) -> Void in
            settings.excludeHidden = !bool
        },
        "listdirs": { (bool: Bool, settings: FindSettings) -> Void in
            settings.listDirs = bool
        },
        "listfiles": { (bool: Bool, settings: FindSettings) -> Void in
            settings.listFiles = bool
        },
        "nocolorize": { (bool: Bool, settings: FindSettings) -> Void in
            settings.colorize = !bool
        },
        "norecursive": { (bool: Bool, settings: FindSettings) -> Void in
            settings.recursive = !bool
        },
        "recursive": { (bool: Bool, settings: FindSettings) -> Void in
            settings.recursive = bool
        },
        "verbose": { (bool: Bool, settings: FindSettings) -> Void in
            settings.verbose = bool
        },
        "version": { (bool: Bool, settings: FindSettings) -> Void in
            settings.printVersion = bool
        },
    ]

    public func settingsFromArgs(_ args: [String], error: NSErrorPointer) -> FindSettings {
        var i = 0
        let settings = FindSettings()
        // default listFiles to true since running as cli
        settings.listFiles = true
        while i < args.count {
            var arg = args[i]
            if arg.hasPrefix("-") {
                while arg.hasPrefix("-"), arg.lengthOfBytes(using: String.Encoding.utf8) > 1 {
                    arg = String(arg[arg.index(arg.startIndex, offsetBy: 1)...])
                }
                if longArgDict.index(forKey: arg) != nil {
                    let longArg = longArgDict[arg]
                    if argActionDict.index(forKey: longArg!) != nil {
                        if args.count > i + 1 {
                            argActionDict[longArg!]!(args[i + 1], settings)
                            i += 1
                        } else {
                            setError(error, msg: "Missing argument for option \(arg)")
                            break
                        }
                    } else if boolFlagActionDict.index(forKey: longArg!) != nil {
                        boolFlagActionDict[longArg!]!(true, settings)
                    } else {
                        setError(error, msg: "Invalid option: \(arg)")
                        break
                    }
                } else {
                    setError(error, msg: "Invalid option: \(arg)")
                    break
                }
            } else {
                settings.addPath(args[i])
            }
            i += 1
        }
        return settings
    }

    public func settingsFromFile(_ filePath: String, error: NSErrorPointer) -> FindSettings {
        let settings = FindSettings()
        addSettingsFromFile(filePath, settings: settings, error: error)
        return settings
    }

    public func addSettingsFromFile(_ filePath: String, settings: FindSettings, error: NSErrorPointer) {
        do {
            let fileUrl = URL(fileURLWithPath: filePath)
            let jsonString = try String(contentsOf: fileUrl, encoding: .utf8)
            addSettingsFromJson(jsonString, settings: settings, error: error)
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    public func settingsFromJson(_ jsonString: String, error: NSErrorPointer) -> FindSettings {
        let settings = FindSettings()
        addSettingsFromJson(jsonString, settings: settings, error: error)
        return settings
    }

    public func addSettingsFromJson(_ jsonString: String, settings: FindSettings, error: NSErrorPointer) {
        do {
            if let json = try JSONSerialization.jsonObject(with: jsonString.data(using: .utf8)!,
                                                           options: []) as? [String: Any]
            {
                for key in json.keys {
                    if longArgDict.index(forKey: key) != nil {
                        let longArg = longArgDict[key]
                        if argActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let string = value as? String {
                                argActionDict[longArg!]!(string, settings)
                            } else if let bool = value as? Bool {
                                argActionDict[longArg!]!(bool.description, settings)
                            } else if let int = value as? Int {
                                argActionDict[longArg!]!(int.description, settings)
                            } else if let stringArray = value as? [String] {
                                for s in stringArray {
                                    argActionDict[longArg!]!(s, settings)
                                }
                            } else {
                                setError(error, msg: "Invalid type for \"\(key)\" entry")
                                break
                            }
                        } else if boolFlagActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let bool = value as? Bool {
                                boolFlagActionDict[longArg!]!(bool, settings)
                            } else {
                                setError(error, msg: "Invalid type for \"\(key)\" entry")
                                break
                            }
                        } else {
                            setError(error, msg: "Invalid option: \(key)")
                            break
                        }
                    } else if key == "path" {
                        let value = json[key]
                        if let string = value as? String {
                            settings.addPath(string)
                        }
                    } else {
                        setError(error, msg: "Invalid option: \(key)")
                        break
                    }
                }
            }
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    public func usage(_ code: Int32 = 0) {
        logMsg(getUsageString())
        exit(code)
    }

    func getUsageString() -> String {
        var str = "\nUsage:\n swiftfind [options] <path> [<path> ...]\n\n"
        str += "Options:\n"
        let optStrings = findOptions.map {
            $0.short.isEmpty ? "--\($0.long)" : "-\($0.short),--\($0.long)"
        }
        let optDescs = findOptions.map(\.desc)
        let longest = optStrings.map { $0.lengthOfBytes(using: String.Encoding.utf8) }.max()!
        for i in 0 ..< optStrings.count {
            var optLine = " \(optStrings[i])"
            while optLine.lengthOfBytes(using: String.Encoding.utf8) <= longest {
                optLine += " "
            }
            optLine += "  \(optDescs[i])\n"
            str += optLine
        }
        return str
    }
}

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

public class FindOptions {
    private var config: Config
    private var findOptions = [FindOption]()
    private var longArgDict: [String: String] = [:]

    public init() {
        self.config = Config()
        setFindOptionsFromJson()
    }

    private func setFindOptionsFromJson() {
        do {
            let findOptionsUrl = URL(fileURLWithPath: config.findOptionsPath)
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
                try? self.addSettingsFromFile(str, settings: settings)
            },
        ]
    }

    private let boolFlagActionDict: [String: (Bool, FindSettings) -> Void] = [
        "archivesonly": { (bool: Bool, settings: FindSettings) -> Void in
            settings.archivesOnly = bool
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

    public func settingsFromArgs(_ args: [String]) throws -> FindSettings {
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
                            throw FindError(msg: "Missing argument for option \(arg)")
                        }
                    } else if boolFlagActionDict.index(forKey: longArg!) != nil {
                        boolFlagActionDict[longArg!]!(true, settings)
                    } else {
                        throw FindError(msg: "Invalid option: \(arg)")
                    }
                } else {
                    throw FindError(msg: "Invalid option: \(arg)")
                }
            } else {
                settings.addPath(args[i])
            }
            i += 1
        }
        return settings
    }

    public func settingsFromFile(_ filePath: String) throws -> FindSettings {
        let settings = FindSettings()
        try addSettingsFromFile(filePath, settings: settings)
        return settings
    }

    public func addSettingsFromFile(_ filePath: String, settings: FindSettings) throws {
        do {
            let fileUrl = URL(fileURLWithPath: filePath)
            let jsonString = try String(contentsOf: fileUrl, encoding: .utf8)
            try addSettingsFromJson(jsonString, settings: settings)
        } catch let error as NSError {
            throw FindError(msg: "Failed to load: \(error.localizedDescription)")
        }
    }

    public func settingsFromJson(_ jsonString: String) throws -> FindSettings {
        let settings = FindSettings()
        try addSettingsFromJson(jsonString, settings: settings)
        return settings
    }

    public func addSettingsFromJson(_ jsonString: String, settings: FindSettings) throws {
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
                                throw FindError(msg: "Invalid type for \"\(key)\" entry")
                            }
                        } else if boolFlagActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let bool = value as? Bool {
                                boolFlagActionDict[longArg!]!(bool, settings)
                            } else {
                                throw FindError(msg: "Invalid type for \"\(key)\" entry")
                            }
                        } else {
                            throw FindError(msg: "Invalid option: \(key)")
                        }
                    } else if key == "path" {
                        let value = json[key]
                        if let string = value as? String {
                            settings.addPath(string)
                        }
                    } else {
                        throw FindError(msg: "Invalid option: \(key)")
                    }
                }
            }
        } catch let error as NSError {
            throw FindError(msg: "Failed to load: \(error.localizedDescription)")
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

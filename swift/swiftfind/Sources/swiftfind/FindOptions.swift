//
//  FindOptions.swift
//  swiftfind
//
//  Created by Cary Clark on 5/17/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

struct FindOption {
    let shortArg: String?
    let longArg: String
    let desc: String

    var sortArg: String {
        if shortArg != nil, !shortArg!.isEmpty {
            return shortArg!.lowercased() + "@" + longArg.lowercased()
        }
        return longArg.lowercased()
    }
}

public class FindOptions {
    private var config: FindConfig
    private var findOptions = [FindOption]()
    private var longArgDict: [String: String] = [:]

    public init() {
        config = FindConfig()
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
                        let shortArg = so.index(forKey: "short") != nil ? (so["short"] as! String) : nil
                        let desc = so["desc"] as! String
                        findOptions.append(FindOption(shortArg: shortArg, longArg: longArg, desc: desc))
                    }
                    for opt in findOptions {
                        longArgDict[opt.longArg] = opt.longArg
                        if opt.shortArg != nil, !opt.shortArg!.isEmpty {
                            longArgDict[opt.shortArg!] = opt.longArg
                        }
                    }
                }
            }
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    private let boolActionDict: [String: (Bool, FindSettings) -> Void] = [
        "archivesonly": { (bool: Bool, settings: FindSettings) in
            settings.archivesOnly = bool
        },
        "debug": { (bool: Bool, settings: FindSettings) in
            settings.debug = bool
        },
        "excludearchives": { (bool: Bool, settings: FindSettings) in
            settings.includeArchives = !bool
        },
        "excludehidden": { (bool: Bool, settings: FindSettings) in
            settings.includeHidden = !bool
        },
        "followsymlinks": { (bool: Bool, settings: FindSettings) in
            settings.followSymlinks = bool
        },
        "help": { (bool: Bool, settings: FindSettings) in
            settings.printUsage = bool
        },
        "includearchives": { (bool: Bool, settings: FindSettings) in
            settings.includeArchives = bool
        },
        "includehidden": { (bool: Bool, settings: FindSettings) in
            settings.includeHidden = bool
        },
        "nofollowsymlinks": { (bool: Bool, settings: FindSettings) in
            settings.followSymlinks = !bool
        },
        "noprintdirs": { (bool: Bool, settings: FindSettings) in
            settings.printDirs = !bool
        },
        "noprintfiles": { (bool: Bool, settings: FindSettings) in
            settings.printFiles = !bool
        },
        "norecursive": { (bool: Bool, settings: FindSettings) in
            settings.recursive = !bool
        },
        "printdirs": { (bool: Bool, settings: FindSettings) in
            settings.printDirs = bool
        },
        "printfiles": { (bool: Bool, settings: FindSettings) in
            settings.printFiles = bool
        },
        "recursive": { (bool: Bool, settings: FindSettings) in
            settings.recursive = bool
        },
        "sort-ascending": { (bool: Bool, settings: FindSettings) in
            settings.sortDescending = !bool
        },
        "sort-caseinsensitive": { (bool: Bool, settings: FindSettings) in
            settings.sortCaseInsensitive = bool
        },
        "sort-casesensitive": { (bool: Bool, settings: FindSettings) in
            settings.sortCaseInsensitive = !bool
        },
        "sort-descending": { (bool: Bool, settings: FindSettings) in
            settings.sortDescending = bool
        },
        "verbose": { (bool: Bool, settings: FindSettings) in
            settings.verbose = bool
        },
        "version": { (bool: Bool, settings: FindSettings) in
            settings.printVersion = bool
        }
    ]

    // this is computed property so that it can reference self
    private var stringActionDict: [String: (String, FindSettings) -> Void] {
        [
            "in-archiveext": { (str: String, settings: FindSettings) in
                settings.addInArchiveExtension(str)
            },
            "in-archivefilepattern": { (str: String, settings: FindSettings) in
                settings.addInArchiveFilePattern(str)
            },
            "in-dirpattern": { (str: String, settings: FindSettings) in
                settings.addInDirPattern(str)
            },
            "in-ext": { (str: String, settings: FindSettings) in
                settings.addInExtension(str)
            },
            "in-filepattern": { (str: String, settings: FindSettings) in
                settings.addInFilePattern(str)
            },
            "in-filetype": { (str: String, settings: FindSettings) in
                settings.addInFileType(str)
            },
            "out-archiveext": { (str: String, settings: FindSettings) in
                settings.addOutArchiveExtension(str)
            },
            "maxlastmod": { (str: String, settings: FindSettings) in
                settings.setMaxLastModFromString(str)
            },
            "minlastmod": { (str: String, settings: FindSettings) in
                settings.setMinLastModFromString(str)
            },
            "out-archivefilepattern": { (str: String, settings: FindSettings) in
                settings.addOutArchiveFilePattern(str)
            },
            "out-dirpattern": { (str: String, settings: FindSettings) in
                settings.addOutDirPattern(str)
            },
            "out-ext": { (str: String, settings: FindSettings) in
                settings.addOutExtension(str)
            },
            "out-filepattern": { (str: String, settings: FindSettings) in
                settings.addOutFilePattern(str)
            },
            "out-filetype": { (str: String, settings: FindSettings) in
                settings.addOutFileType(str)
            },
            "path": { (str: String, settings: FindSettings) in
                settings.addPath(str)
            },
            "settings-file": { (str: String, settings: FindSettings) in
                try? self.addSettingsFromFile(str, settings: settings)
            },
            "sort-by": { (str: String, settings: FindSettings) in
                settings.setSortBy(str)
            }
        ]
    }

    private let intActionDict: [String: (Int32, FindSettings) -> Void] = [
        "maxdepth": { (i: Int32, settings: FindSettings) in
            settings.maxDepth = i
        },
        "mindepth": { (i: Int32, settings: FindSettings) in
            settings.minDepth = i
        },
    ]

    private let longActionDict: [String: (UInt64, FindSettings) -> Void] = [
        "maxsize": { (l: UInt64, settings: FindSettings) in
            settings.maxSize = l
        },
        "minsize": { (l: UInt64, settings: FindSettings) in
            settings.minSize = l
        },
    ]

    public func settingsFromArgs(_ args: [String]) throws -> FindSettings {
        var i = 0
        let settings = FindSettings()
        // default printFiles to true since running as cli
        settings.printFiles = true
        while i < args.count {
            var arg = args[i]
            if arg.hasPrefix("-") {
                while arg.hasPrefix("-"), arg.lengthOfBytes(using: String.Encoding.utf8) > 1 {
                    arg = String(arg[arg.index(arg.startIndex, offsetBy: 1)...])
                }
                if longArgDict.index(forKey: arg) != nil {
                    let longArg = longArgDict[arg]
                    if boolActionDict.index(forKey: longArg!) != nil {
                        boolActionDict[longArg!]!(true, settings)
                    } else {
                        var argVal = ""
                        if args.count > i + 1 {
                            i += 1
                            argVal = args[i]
                        } else {
                            throw FindError(msg: "Missing argument for option \(arg)")
                        }
                        
                        if stringActionDict.index(forKey: longArg!) != nil {
                            stringActionDict[longArg!]!(argVal, settings)
                        } else if intActionDict.index(forKey: longArg!) != nil {
                            let intVal = Int32(argVal) ?? 0
                            intActionDict[longArg!]!(intVal, settings)
                        } else if longActionDict.index(forKey: longArg!) != nil {
                            let longVal = UInt64(argVal) ?? 0
                            longActionDict[longArg!]!(longVal, settings)
                        } else {
                            throw FindError(msg: "Invalid option: \(arg)")
                        }
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
                        if boolActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let bool = value as? Bool {
                                boolActionDict[longArg!]!(bool, settings)
                            } else {
                                throw FindError(msg: "Invalid type for \"\(key)\" entry")
                            }
                        } else if stringActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let string = value as? String {
                                stringActionDict[longArg!]!(string, settings)
                            } else if let bool = value as? Bool {
                                stringActionDict[longArg!]!(bool.description, settings)
                            } else if let int = value as? Int {
                                stringActionDict[longArg!]!(int.description, settings)
                            } else if let stringArray = value as? [String] {
                                for s in stringArray {
                                    stringActionDict[longArg!]!(s, settings)
                                }
                            } else {
                                throw FindError(msg: "Invalid type for \"\(key)\" entry")
                            }
                        } else if intActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let intVal = value as? Int32 {
                                intActionDict[longArg!]!(intVal, settings)
                            } else {
                                throw FindError(msg: "Invalid type for \"\(key)\" entry")
                            }
                        } else if longActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let longVal = value as? UInt64 {
                                longActionDict[longArg!]!(longVal, settings)
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
        findOptions.sort(by: { $0.sortArg < $1.sortArg })

        let optStrings = findOptions.map {
            switch ($0.shortArg, $0.longArg) {
            // Order errors by code
            case let (nil, longArg):
                "--\(longArg)"
            case let ("", longArg):
                "--\(longArg)"
            case let (shortArg, longArg):
                "-\(shortArg!),--\(longArg)"
            }
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

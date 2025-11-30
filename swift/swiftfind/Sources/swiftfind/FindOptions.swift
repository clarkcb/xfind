//
//  FindOptions.swift
//  swiftfind
//
//  Created by Cary Clark on 5/17/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public protocol Option {
    var shortArg: String? { get }
    var longArg: String { get }
    var desc: String { get }
    var argType: ArgTokenType { get }
}

struct FindOption: Option {
    let shortArg: String?
    let longArg: String
    let desc: String
    let argType: ArgTokenType

    var sortArg: String {
        if shortArg != nil, !shortArg!.isEmpty {
            return shortArg!.lowercased() + "a" + longArg.lowercased()
        }
        return longArg.lowercased()
    }
}

public class FindOptions {
    private var config: FindConfig
    private var findOptions = [FindOption]()
    private var argTokenizer: ArgTokenizer?

    public init() {
        config = FindConfig()
        setFindOptionsFromJson()
        argTokenizer = ArgTokenizer(findOptions)
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
                        var argType = ArgTokenType.unknown
                        if self.boolActionDict.index(forKey: longArg) != nil {
                            argType = ArgTokenType.bool
                        } else if self.stringActionDict.index(forKey: longArg) != nil {
                            argType = ArgTokenType.str
                        } else if self.intActionDict.index(forKey: longArg) != nil {
                            argType = ArgTokenType.int
                        } else if self.longActionDict.index(forKey: longArg) != nil {
                            argType = ArgTokenType.long
                        }
                        findOptions.append(FindOption(shortArg: shortArg, longArg: longArg, desc: desc, argType: argType))
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
        "colorize": { (bool: Bool, settings: FindSettings) in
            settings.colorize = bool
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
        "nocolorize": { (bool: Bool, settings: FindSettings) in
            settings.colorize = !bool
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

    private let stringActionDict: [String: (String, FindSettings) -> Void] = [
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
        "sort-by": { (str: String, settings: FindSettings) in
            settings.setSortBy(str)
        }
    ]

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

    public func updateSettingsFromArgTokens(_ settings: FindSettings, argTokens: [ArgToken]) throws {
        for argToken in argTokens {
            if argToken.type == ArgTokenType.bool {
                if let bool = argToken.value as? Bool {
                    boolActionDict[argToken.name]!(bool, settings)
                } else {
                    throw FindError(msg: "Invalid value for option: \(argToken.name)")
                }
            } else if argToken.type == ArgTokenType.str {
                if let string = argToken.value as? String {
                    if argToken.name == "settings-file" {
                        try updateSettingsFromFile(settings, filePath: string)
                    } else {
                        stringActionDict[argToken.name]!(string, settings)
                    }
                } else if let stringArray = argToken.value as? [String] {
                    for s in stringArray {
                        stringActionDict[argToken.name]!(s, settings)
                    }
                } else {
                    throw FindError(msg: "Invalid value for option: \(argToken.name)")
                }
            } else if argToken.type == ArgTokenType.int {
                if let intVal = argToken.value as? Int32 {
                    intActionDict[argToken.name]!(intVal, settings)
                } else {
                    throw FindError(msg: "Invalid value for option: \(argToken.name)")
                }
            }  else if argToken.type == ArgTokenType.long {
                if let longVal = argToken.value as? UInt64 {
                    longActionDict[argToken.name]!(longVal, settings)
                } else {
                    throw FindError(msg: "Invalid value for option: \(argToken.name)")
                }
            } else {
                throw FindError(msg: "Invalid option: \(argToken.name)")
            }
        }
    }

    public func updateSettingsFromJson(_ settings: FindSettings, jsonString: String) throws {
        let argTokens = try argTokenizer!.tokenizeJson(jsonString)
        try updateSettingsFromArgTokens(settings, argTokens: argTokens)
    }

    public func settingsFromJson(_ jsonString: String) throws -> FindSettings {
        let settings = FindSettings()
        try updateSettingsFromJson(settings, jsonString: jsonString)
        return settings
    }

    public func updateSettingsFromFile(_ settings: FindSettings, filePath: String) throws {
        let argTokens = try argTokenizer!.tokenizeFile(filePath)
        try updateSettingsFromArgTokens(settings, argTokens: argTokens)
    }

    public func settingsFromFile(_ filePath: String) throws -> FindSettings {
        let settings = FindSettings()
        try updateSettingsFromFile(settings, filePath: filePath)
        return settings
    }

    public func updateSettingsFromArgs(_ settings: FindSettings, args: [String]) throws {
        let argTokens = try argTokenizer!.tokenizeArgs(args)
        try updateSettingsFromArgTokens(settings, argTokens: argTokens)
    }

    public func settingsFromArgs(_ args: [String]) throws -> FindSettings {
        let settings = FindSettings()
        // default printFiles to true since running in cli
        settings.printFiles = true
        try updateSettingsFromArgs(settings, args: args)
        return settings
    }

    func getUsageString() -> String {
        var str = "\nUsage:\n swiftfind [options] <path> [<path> ...]\n\n"
        str += "Options:\n"
        let options = findOptions.sorted(by: { $0.sortArg < $1.sortArg })

        let optStrings = options.map {
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

        let optDescs = options.map(\.desc)
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

    public func usage(_ code: Int32 = 0) {
        logMsg(getUsageString())
        exit(code)
    }
}

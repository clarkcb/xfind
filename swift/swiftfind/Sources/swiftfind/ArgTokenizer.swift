//
//  ArgParser.swift
//  swiftfind
//
//  Created by Cary Clark on 9/1/25.
//  Copyright (c) 2025 Cary Clark. All rights reserved.
//

import Foundation

public enum ArgTokenType {
    case bool, str, int, long
}

public struct ArgToken {
    let name: String
    let type: ArgTokenType
    let value: Any
}

public class ArgTokenizer {
    private var boolDict: [String: String] = [:]
    private var stringDict: [String: String] = [:]
    private var intDict: [String: String] = [:]
    private var longDict: [String: String] = [:]

    public init(_ boolDict: [String: String], _ stringDict: [String: String], _ intDict: [String: String], _ longDict: [String: String]) {
        self.boolDict = boolDict
        self.stringDict = stringDict
        self.intDict = intDict
        self.longDict = longDict
    }

    public func tokenizeArgs(_ args: [String]) throws -> [ArgToken] {
        var i = 0
        var argTokens: [ArgToken] = []
        // default printFiles to true since running as cli
        while i < args.count {
            var arg = args[i]
            if arg.hasPrefix("-") {
                var argNames: [String] = []
                var argVal: String? = nil
                if arg.hasPrefix("--") {
                    if arg.count > 2 {
                        // Process long arg
                        arg = String(arg[arg.index(arg.startIndex, offsetBy: 2)...])
                        if arg.contains("=") {
                            let parts = arg.split(separator: "=")
                            if parts.count > 0 {
                                arg = String(parts[0])
                            }
                            if parts.count > 1 {
                                argVal = String(parts[1])
                            }
                        }
                        argNames.append(arg)
                    } else {
                        throw FindError(msg: "Invalid option: \(arg)")
                    }
                } else if arg.count > 1 {
                    // Process short arg(s)
                    arg = String(arg[arg.index(arg.startIndex, offsetBy: 1)...])
                    for c in arg {
                        let cs = String(c)
                        if boolDict.index(forKey: cs) != nil {
                            argNames.append(boolDict[cs]!)
                        } else if stringDict.index(forKey: cs) != nil {
                            argNames.append(stringDict[cs]!)
                        } else if intDict.index(forKey: cs) != nil {
                            argNames.append(intDict[cs]!)
                        } else if longDict.index(forKey: cs) != nil {
                            argNames.append(longDict[cs]!)
                        } else {
                            throw FindError(msg: "Invalid option: \(cs)")
                        }
                    }
                } else {
                    throw FindError(msg: "Invalid option: \(arg)")
                }
                
                for argName in argNames {
                    if boolDict.index(forKey: argName) != nil {
                        argTokens.append(ArgToken(name: argName, type: ArgTokenType.bool, value: true))
                    } else {
                        if argVal == nil {
                            if args.count > i + 1 {
                                i += 1
                                argVal = args[i]
                            } else {
                                throw FindError(msg: "Missing argument for option \(arg)")
                            }
                        }
                        if stringDict.index(forKey: argName) != nil {
                            argTokens.append(ArgToken(name: argName, type: ArgTokenType.str, value: argVal!))
                        } else if intDict.index(forKey: argName) != nil {
                            let intVal = Int32(argVal!) ?? 0
                            argTokens.append(ArgToken(name: argName, type: ArgTokenType.int, value: intVal))
                        } else if longDict.index(forKey: argName) != nil {
                            let longVal = UInt64(argVal!) ?? 0
                            argTokens.append(ArgToken(name: argName, type: ArgTokenType.long, value: longVal))
                        } else if argName == "settings-file" {
                            argTokens.append(ArgToken(name: argName, type: ArgTokenType.str, value: argVal!))
                        } else {
                            throw FindError(msg: "Invalid option: \(arg)")
                        }
                    }
                }
            } else {
                argTokens.append(ArgToken(name: "path", type: ArgTokenType.str, value: arg))
            }
            i += 1
        }
        return argTokens
    }

    public func tokenizeArgMap(_ argMap: [String: Any]) throws -> [ArgToken] {
        var argTokens: [ArgToken] = []
        // keys are sorted so that output is consistent across all versions
        let keys = argMap.keys.sorted()
        for key in keys {
            if boolDict.index(forKey: key) != nil {
                let value = argMap[key]
                if let bool = value as? Bool {
                    argTokens.append(ArgToken(name: key, type: ArgTokenType.bool, value: bool))
                } else {
                    throw FindError(msg: "Invalid value for option: \(key)")
                }
            } else if stringDict.index(forKey: key) != nil || key == "settings-file" {
                let value = argMap[key]
                if let string = value as? String {
                    argTokens.append(ArgToken(name: key, type: ArgTokenType.str, value: string))
                } else if let stringArray = value as? [String] {
                    for s in stringArray {
                        argTokens.append(ArgToken(name: key, type: ArgTokenType.str, value: s))
                    }
                } else {
                    throw FindError(msg: "Invalid value for option: \(key)")
                }
            } else if intDict.index(forKey: key) != nil {
                let value = argMap[key]
                if let intVal = value as? Int32 {
                    argTokens.append(ArgToken(name: key, type: ArgTokenType.int, value: intVal))
                } else {
                    throw FindError(msg: "Invalid value for option: \(key)")
                }
            } else if longDict.index(forKey: key) != nil {
                let value = argMap[key]
                if let longVal = value as? UInt64 {
                    argTokens.append(ArgToken(name: key, type: ArgTokenType.long, value: longVal))
                } else {
                    throw FindError(msg: "Invalid value for option: \(key)")
                }
            } else {
                throw FindError(msg: "Invalid option: \(key)")
            }
        }
        return argTokens
    }

    public func tokenizeJson(_ jsonString: String) throws -> [ArgToken] {
        var argTokens: [ArgToken] = []
        do {
            if let json = try JSONSerialization.jsonObject(with: jsonString.data(using: .utf8)!,
                                                           options: []) as? [String: Any]
            {
                argTokens = try tokenizeArgMap(json)
            }
        } catch let error as FindError {
            throw error
        } catch let error {
            throw FindError(msg: "Failed to load: \(error.localizedDescription)")
        }
        return argTokens
    }

    public func tokenizeFile(_ filePath: String) throws -> [ArgToken] {
        var argTokens: [ArgToken] = []
        let expandedPath = FileUtil.expandPath(filePath)
        if !FileUtil.exists(expandedPath) {
            throw FindError(msg: "Settings file not found: \(filePath)")
        }
        if !expandedPath.hasSuffix(".json") {
            throw FindError(msg: "Invalid settings file (must be JSON): \(filePath)")
        }
        do {
            let fileUrl = URL(fileURLWithPath: expandedPath)
            let jsonString = try String(contentsOf: fileUrl, encoding: .utf8)
            argTokens = try tokenizeJson(jsonString)
        } catch let error as FindError {
            throw error
        } catch let error {
            throw FindError(msg: "Failed to load: \(error.localizedDescription)")
        }
        return argTokens
    }
}

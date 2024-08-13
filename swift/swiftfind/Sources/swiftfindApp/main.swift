//
//  main.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
import swiftfind

func getMatchingDirs(_ fileResults: [FileResult]) -> [String] {
    fileResults.map {
        URL(fileURLWithPath: $0.filePath).deletingLastPathComponent().path
    }.sorted().unique()
}

func getMatchingFiles(_ fileResults: [FileResult]) -> [String] {
    fileResults.map(\.filePath)
}

func handleError(_ error: FindError, _ options: FindOptions) {
    logMsg("")
    logError(error.msg)
    options.usage(1)
}

func main() {
    let options = FindOptions()

    let args: [String] = [] + CommandLine.arguments.dropFirst()

    do {
        let settings = try options.settingsFromArgs(args)

        if settings.debug {
            logMsg("\nsettings: \(settings)")
        }

        if settings.printUsage {
            options.usage()
        }

        let finder = try Finder(settings: settings)

        let fileResults = try finder.find()

        if settings.printDirs {
            let dirs = getMatchingDirs(fileResults)
            if dirs.isEmpty {
                logMsg("\nMatching directories: 0")
            } else {
                logMsg("\nMatching directories (\(dirs.count)):")
                for dir in dirs {
                    logMsg(FileUtil.formatPath(dir, forPaths: Array(settings.paths)))
                }
            }
        }

        if settings.printFiles {
            let files = getMatchingFiles(fileResults)
            if files.isEmpty {
                logMsg("\nMatching files: 0")
            } else {
                logMsg("\nMatching files (\(files.count)):")
                for file in files {
                    logMsg(FileUtil.formatPath(file, forPaths: Array(settings.paths)))
                }
            }
        }
    } catch let error as FindError {
        handleError(error, options)
    } catch {
        logError("Unknown error occurred")
    }
}

main()

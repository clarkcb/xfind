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
        let formatter = FileResultFormatter(settings: settings)

        if settings.printDirs {
            let dirs = getMatchingDirs(fileResults)
            if dirs.isEmpty {
                logMsg("\nMatching directories: 0")
            } else {
                logMsg("\nMatching directories (\(dirs.count)):")
                for dir in dirs {
                    logMsg(formatter.formatDirPath(dir))
                }
            }
        }

        if settings.printFiles {
            if fileResults.isEmpty {
                logMsg("\nMatching files: 0")
            } else {
                logMsg("\nMatching files (\(fileResults.count)):")
                for fileResult in fileResults {
                    logMsg(formatter.formatFileResult(fileResult))
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

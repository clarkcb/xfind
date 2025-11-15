//
//  main.swift
//  swiftfind
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
import swiftfind

func handleError(_ error: FindError, _ colorize: Bool, _ options: FindOptions) {
    logMsg("")
    logError(error.msg, colorize: colorize)
    options.usage(1)
}

func main() {
    var colorize = true
    let options = FindOptions()

    let args: [String] = [] + CommandLine.arguments.dropFirst()

    do {
        let settings = try options.settingsFromArgs(args)
        colorize = settings.colorize

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
            finder.printMatchingDirs(fileResults, formatter)
        }

        if settings.printFiles {
            finder.printMatchingFiles(fileResults, formatter)
        }
    } catch let error as FindError {
        handleError(error, colorize, options)
    } catch {
        logError("Unknown error occurred")
    }
}

main()

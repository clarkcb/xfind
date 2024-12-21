package groovyfind

import java.util.stream.Collectors

class GroovyFind {

    private static void handleError(final String message) {
        Logger.log('')
        Logger.logError(message)
    }

    private static void handleError(final String message, final FindOptions options) {
        Logger.log('')
        Logger.logError(message + '\n')
        options.usage(1)
    }

    private static List<String> getMatchingDirs(final List<FileResult> fileResults) {
        fileResults.collect { fr -> fr.path.parent.toString() }.unique().sort()
    }

    private static void printMatchingDirs(final List<FileResult> fileResults) {
        List<String> dirs = getMatchingDirs(fileResults)
        if (!dirs.empty) {
            Logger.log("\nMatching directories (${dirs.size()}):")
            dirs.each { Logger.log(it) }
        } else {
            Logger.log('\nMatching directories: 0')
        }
    }

    private static List<String> getMatchingFiles(final List<FileResult> fileResults) {
        fileResults.stream().map(FileResult::toString).collect(Collectors.toList())
    }

    private static void printMatchingFiles(final List<FileResult> fileResults) {
        List<String> files = getMatchingFiles(fileResults)
        if (!files.empty) {
            Logger.log("\nMatching files (${files.size()}):")
            files.each { Logger.log(it) }
        } else {
            Logger.log('\nMatching files: 0')
        }
    }

    static void main(final String[] args) {
        try {
            FindOptions options = new FindOptions()

            try {
                FindSettings settings = options.settingsFromArgs(args)

                if (settings.debug) {
                    Logger.log('\nsettings: ' + settings.toString() + '\n')
                }

                if (settings.printUsage) {
                    Logger.log('')
                    options.usage(0)
                }

                Finder finder = new Finder(settings)
                finder.validateSettings()
                List<FileResult> fileResults = finder.find()

                if (settings.printDirs) {
                    printMatchingDirs(fileResults)
                }
                if (settings.printFiles) {
                    printMatchingFiles(fileResults)
                }
            } catch (FindException e) {
                handleError(e.message, options)
            }
        } catch (IOException e) {
            handleError(e.message)
        }
    }
}

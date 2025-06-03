package groovyfind

import java.nio.file.Path
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

    private static List<Path> getMatchingDirs(final List<FileResult> results) {
        results.findAll { fr -> fr.path.parent != null }
                .collect { fr -> fr.path.parent }.unique().sort()
    }

    private static void printMatchingDirs(final List<FileResult> results, final FindSettings settings) {
        List<Path> dirs = getMatchingDirs(results)
        if (!dirs.empty) {
            Logger.log("\nMatching directories (${dirs.size()}):")
            if (settings.getColorize() && !settings.getInDirPatterns().isEmpty()) {
                var formatter = new FileResultFormatter(settings)
                dirs.each { Logger.log(formatter.formatDirPath(it)) }
            } else {
                dirs.each { Logger.log(it.toString()) }
            }
        } else {
            Logger.log('\nMatching directories: 0')
        }
    }

    private static List<String> getMatchingFiles(final List<FileResult> results) {
        results.stream().map(FileResult::toString).collect(Collectors.toList())
    }

    private static void printMatchingFiles(final List<FileResult> results, final FindSettings settings) {
        if (!results.isEmpty()) {
            Logger.log(String.format("\nMatching files (%d):", results.size()))
            if (settings.getColorize() && (!settings.getInDirPatterns().isEmpty()
                    || !settings.getInExtensions().isEmpty() || !settings.getInFilePatterns().isEmpty())) {
                var formatter = new FileResultFormatter(settings)
                for (var f : results) {
                    Logger.log(formatter.formatFileResult(f))
                }
            } else {
                var files = getMatchingFiles(results)
                for (var f : files) {
                    Logger.log(f)
                }
            }
        } else {
            Logger.log("\nMatching files: 0")
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
                    printMatchingDirs(fileResults, settings)
                }
                if (settings.printFiles) {
                    printMatchingFiles(fileResults, settings)
                }
            } catch (FindException e) {
                handleError(e.message, options)
            }
        } catch (IOException e) {
            handleError(e.message)
        }
    }
}

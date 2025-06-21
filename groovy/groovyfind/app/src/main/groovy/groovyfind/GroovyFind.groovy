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
                var formatter = new FileResultFormatter(settings)

                if (settings.printDirs) {
                    finder.printMatchingDirs(fileResults, formatter)
                }
                if (settings.printFiles) {
                    finder.printMatchingFiles(fileResults, formatter)
                }
            } catch (FindException e) {
                handleError(e.message, options)
            }
        } catch (IOException e) {
            handleError(e.message)
        }
    }
}

package groovyfind

class GroovyFind {

    private static void handleError(final String message) {
        Logger.log('')
        Logger.logError(message)
    }

    private static void handleError(final String message, final boolean colorize, final FindOptions options) {
        Logger.log('')
        Logger.logError(message + '\n', colorize)
        options.usage(1)
    }

    static void main(final String[] args) {
        var colorize = true
        try {
            FindOptions options = new FindOptions()

            try {
                FindSettings settings = options.settingsFromArgs(args)
                colorize = settings.colorize

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
                    Finder.printMatchingDirs(fileResults, formatter)
                }
                if (settings.printFiles) {
                    Finder.printMatchingFiles(fileResults, formatter)
                }
            } catch (FindException e) {
                handleError(e.message, colorize, options)
            }
        } catch (IOException e) {
            handleError(e.message)
        }
    }
}

/*******************************************************************************
JavaFind

Main class for initiating javafind from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import java.io.IOException;

import static javafind.Logger.log;
import static javafind.Logger.logError;

public class JavaFind {

    private static void handleError(final String message) {
        log("");
        logError(message);
    }

    private static void handleError(final String message, final FindOptions options) {
        log("");
        logError(message + "\n");
        options.usage(1);
    }

    public static void main(final String[] args) {
        try {
            var options = new FindOptions();

            try {
                var settings = options.settingsFromArgs(args);

                if (settings.getDebug()) {
                    log("\nsettings: " + settings + "\n");
                }

                if (settings.getPrintUsage()) {
                    log("");
                    options.usage(0);
                }

                var finder = new Finder(settings);
                finder.validateSettings();
                var fileResults = finder.find();
                var formatter = new FileResultFormatter(settings);

                if (settings.getPrintDirs()) {
                    Finder.printMatchingDirs(fileResults, formatter);
                }
                if (settings.getPrintFiles()) {
                    Finder.printMatchingFiles(fileResults, formatter);
                }

            } catch (FindException e) {
                handleError(e.getMessage(), options);
            }

        } catch (IOException e) {
            handleError(e.getMessage());
        }
    }
}
